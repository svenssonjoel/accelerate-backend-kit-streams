{-# LANGUAGE CPP, NamedFieldPuns #-}

-- | The program defined by this module responds to the following ENVIRONMENT VARIABLES:
--  
--  * ALLTESTS=0/1     -- run all tests registered
--  * ONEDIMTESTS=1    -- run one dimensional tests (default 1)
--  * MULTIDIMTESTS=0  -- run multidimensional tests as well as single dimensional ones.
--  * USETESTS=0       -- run tests with Use as well as Generate
--
--  * REPACK=0         -- run tests through the full accelerate wrapper, including repacking the results
--
-- Note that JIT.hs also uses env vars "SIMPLEINTERP", and "EMITC".

module Main where 

import           Control.Exception
import           Control.Monad (when)
import qualified Data.Array.Accelerate             as A
-- import qualified Data.Array.Accelerate.Interpreter as I
import           Data.Array.Accelerate.BackendKit.Tests (allProgs,allProgsMap,testCompiler,TestEntry(..),AccProg(AccProg),makeTestEntry)
import           Data.Array.Accelerate.BackendKit.CompilerPipeline (phase0, phase1, phase2, repackAcc)
-- import           Data.Array.Accelerate.BackendKit.CompilerPipeline (phase1)
import           Data.Map           as M
import           Data.Set           as S
import           Data.List          as L
import           Data.Char          (toLower)
import           Test.Framework     (defaultMain, buildTest, testGroup, Test, optionsDescription)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit         ((~?))
import           Test.HUnit as HU
import           System.IO.Unsafe   (unsafePerformIO)
import           System.Environment (getEnvironment, getArgs, withArgs)
import           System.Console.GetOpt

import qualified Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as SimpleAcc

import GHC.Conc (threadDelay)
import Debug.Trace        (trace)
import qualified Data.Array.Accelerate.Cilk.JITRuntime   as JIT (run,rawRunIO)
import qualified Data.Array.Accelerate.Cilk as CilkRun
import qualified Data.Array.Accelerate.C    as CRun
import           Data.Array.Accelerate.Shared.EmitC (ParMode(..))

#ifdef ENABLE_OPENCL
import qualified Data.Array.Accelerate.OpenCL.JITRuntime as OpenCL (run,rawRunIO) 
#endif
--------------------------------------------------------------------------------  

defaultMode :: RunMode
#ifdef ENABLE_OPENCL
defaultMode = SequentialC
#elif defined(SEQUENTIAL_DEFAULT)
defaultMode = SequentialC
#else
defaultMode = Cilk
#endif


#ifdef ENABLE_OPENCL
rawRunOpenCL name test = do 
   x <- OpenCL.rawRunIO name test
   -- HACK: sleep to let opencl shut down.
   -- threadDelay 1000000
   return x

runOpenCL = OpenCL.run

#else 
rawRunOpenCL name test = error "Not compiled with OpenCL support"
runOpenCL = error "Not compiled with OpenCL support"
#endif

--------------------------------------------------------------------------------

data Flag = Help | SetMode RunMode | NoRepack     deriving (Eq,Ord,Show,Read)
data RunMode = SequentialC | Cilk | OpenCL        deriving (Eq,Ord,Show,Read)

options :: [OptDescr Flag]
options =
     [ Option ['h'] ["help"] (NoArg Help)              "report this help message"
     , Option [] ["cilk"]    (NoArg (SetMode Cilk))    "use parallel Cilk C backend"
     , Option [] ["seq"]     (NoArg (SetMode SequentialC)) "use sequential C backend"
     , Option [] ["opencl"]  (NoArg (SetMode OpenCL))  "use OpenCL backend, if compiled with support"
     , Option [] ["norepack"]  (NoArg NoRepack)  "do NOT run tests through the full accelerate wrapper, repacking the results"
     ]

main :: IO ()
main = do

 args <- getArgs
 let (opts,nonopts,unrecog,errs) = getOpt' Permute options args
 -- let (opts,nonopts,errs) = getOpt Permute options args 
 let help1 = usageInfo ("USAGE: test-accelerate-cpu-* [options]\n"++
                       "\nFirst, specific options for test harness are:\n"++
                       "---------------------------------------------\n")
               options
     help2 = usageInfo (help1++"\nAlso use the generic test-framework options below:\n"++
                       "--------------------------------------------------\n")
                       optionsDescription

     themode = L.foldl fn defaultMode opts
     fn _ (SetMode m) = m
     fn acc _         = acc
 
 if Help `elem` opts || errs /= [] then error help2
  else do
   let passthru = nonopts ++ unrecog
   -- let passthru = args
   putStrLn$ "  [Note: passing through options to test-framework]: "++unwords passthru
   withArgs passthru $ do 
    ------------------------------------------------------------  
    putStrLn$ " [!] Testing backend: "++ show themode
    ----------------------------------------  
    putStrLn "[main] First checking that all requested tests can be found within 'allProgs'..."
    -- A tuple of the descriptive name and simple name for each test:
    let supportedTestNames =
          [ ("oneDim_"++x,x)   | x <- oneDimOrLessTests ] ++
          [ ("useTest_"++x,x)  | x <- useTests ] ++
          [ ("multiDim_"++x,x) | x <- multiDimTests ] ++
          [ ("highDim_"++x,x)  | x <- highDimTests ]

    let allMentioned = S.fromList $ oneDimOrLessTests ++ useTests ++ multiDimTests ++ highDimTests
    let notMentioned = S.toList$ S.difference (M.keysSet allProgsMap) allMentioned

    let manualExamples = [example] 
         -- Demonstration: we manually put in additional programs to test.
         -- You may add more tests here as well.
    let supportedTests :: [TestEntry]
        supportedTests = 
          manualExamples ++
          L.map (\ (nm,t) -> case M.lookup t allProgsMap of 
                          Nothing -> error$"Test not found: "++ show t
                          -- HACK: appending ":" as an end-marker to all test names.  This 
                          -- makes it possible to use the -t pattern matching for all tests:
                          Just te -> (nameHackTE nm te))
                supportedTestNames
    if supportedTests == [] then error$ "supportedTestNames should not be null" else return ()
    evaluate (L.foldl1 seq $ L.map (\(TestEntry _ _ _ _) -> ()) supportedTests)
    putStrLn$"[main] Yep, all "++show (length supportedTests)++" tests are there."
    ----------------------------------------  

    let rawComp name test = 
          case themode of
            Cilk        -> JIT.rawRunIO CilkParallel name (phase2 test)
            SequentialC -> JIT.rawRunIO Sequential   name (phase2 test)
            OpenCL      -> rawRunOpenCL name test 
    let testsPlain = testCompiler (\ name test -> unsafePerformIO$ rawComp name test) supportedTests
    let testsRepack = 
          L.zipWith (\ i (TestEntry name _ _ (AccProg prg)) ->
                      let conf = CRun.defaultConf {CRun.dbgName = Just name}
                          runit = case themode of
                                   OpenCL      -> runOpenCL
                                   Cilk        -> CilkRun.runDetailed conf
                                   SequentialC -> CRun.runDetailed conf
                          str = show (runit prg)
                          iotest :: IO Bool
                          iotest = do evaluate str
                                      return (length str > 0)
                      in testGroup ("run test "++show i++" "++name) $
                         hUnitTestToTests (iotest ~? "non-empty result string")
                    )
          [1::Int ..] supportedTests

    let goodTests = if NoRepack `elem` opts 
                    then testsPlain 
                    else testsRepack  -- DEFAULT
    let badTests =  concat 
                    [ hUnitTestToTests $ HU.TestLabel ("unhandled_"++nm) $ 
                      HU.TestCase $ assertException [""] $ do
                        x <- rawComp nm simpleProg
                        let str = show$ concatMap SimpleAcc.arrPayloads x
                        if (str == result) then
                          putStrLn $ "WARNING: supposedly unhandled test case got the right answer: "++show nm
                         else do
                           putStrLn $ "WARNING: unsupported test case fails with a wrong answer rather than exception: "++show nm
                           putStrLn $ "Received "++str
                           putStrLn $ "Expected: "++result
                        assertEqual "answer for unhandled test" result str
                    | nm <- notMentioned
                    , let TestEntry{simpleProg,result} = allProgsMap M.! nm ]

    defaultMain (goodTests ++ badTests)
    putStrLn " [Test.hs] You will never see this message, because test-framework defaultMain exits the process."

-- Add an extra terminating character to make the "-t" command line option more
-- useful for selecting tests.
nameHack :: String -> String
nameHack = (++":")

nameHackTE :: String -> TestEntry -> TestEntry
nameHackTE nm (TestEntry _ prg ans orig) = (TestEntry (nameHack nm) prg ans orig)

unNameHack :: String -> String
unNameHack = init 


example :: TestEntry
example = makeTestEntry "example" p
 where
   p :: A.Acc (A.Scalar Int)
   p = A.unit 3 

oneDimOrLessTests :: [String]
oneDimOrLessTests = words$ 
     " p1a p1aa p1ab p1ac p1ba  "
  ++ " p2 "                    -- These will push map through replicate     
  ++ " p2a  p2e p2f "          -- Some simple replicates
  ++ " p16a p16b p16c p16d"
  ++ " p16e"                   -- Has a map that won't get fused; gets desugared to Generate.
  ++ " p4 p4c"                 -- scalar indexing
  ++ " p6b"                    -- scalar tuples
--  ++ " p9a p9b p9c"            -- array of tuples    
  ++ " p10c p10d p10e p10f p10g p10h p10i "  -- uses slice/index
  ++ " p11 p11b p11c  "                      -- tuples of arrays
  ++ " p12 p12b p12c p12d p12e"              -- array and scalar conditionals
  ++ " p17a "                                -- uses trivial reshape
  ++ " p18a p18b p18d p18e p18f "            -- dynamically sized array

  ++ " p1 " -- This adds a FOLD.
  ++ " p1d p6 " -- Array of tuples

-- These tests are waiting on arrays of tuples:

  -- DUMPING these in, go through them:
  ++ "p5 p8 p9a p9b  p14d p14c "
  -- p9c p13 p14e

  -- Scalar tuples and projection:
  -- KNOWN problems with tuple packing presently:
  -- ++ "p13 p13b p13c p13d p13e p13f p14 p14b p14e "
  -- ++ "p9c"

useTests :: [String]
useTests = words$ 
  " p0 p1c " ++
  -- These are ALSO multidim:
  " p7 "

-- | Two and three dimensional tests.
multiDimTests :: [String]
multiDimTests = words$ 
  "p2aa p2bb " ++ 
  "p2b p2c p2cc p2cd p2ce "++ -- a Replicate with an 'All'.   
  "p3 p4b " ++
  "p10 p10b " ++ 
  "p17b " ++
--  "p20a p20b p20c " ++  -- fold1seg
  "p1b p1bb "++ -- fold 2D to 1D
  "p2g p2h " -- Multidim and array-of-tuple

-- | Four dimensional and above.
highDimTests :: [String]
highDimTests = words$ 
  " p18c " ++ -- This internally uses an array-of-tuples but it ends up being dead code.
  " p2i  "++
   "p2d " -- requires array-of-tuple AND >3D

   
------------------------------------------------------------
-- Tests we can't handle yet:
------------------------------------------------------------

known_problems :: [String]
known_problems = []


------------------------------------------------------------
-- Helpers copied from elsewhere:
------------------------------------------------------------

-- | Ensure that executing an action returns an exception
-- containing one of the expected messages.
assertException  :: [String] -> IO a -> IO ()
assertException msgs action = do
 x <- catch (do action; return Nothing) 
            (\e -> do putStrLn $ "Good.  Caught exception: " ++ show (e :: SomeException)
                      return (Just $ show e))
 case x of 
  Nothing -> HU.assertFailure "Failed to get an exception!"
  Just s -> 
   if  any (`isInfixOf` s) msgs
   then return () 
   else HU.assertFailure $ "Got the wrong exception, expected one of the strings: "++ show msgs
        ++ "\nInstead got this exception:\n  " ++ show s

