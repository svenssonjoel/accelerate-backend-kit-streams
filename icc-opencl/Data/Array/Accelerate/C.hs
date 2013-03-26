
-- DUPLICATED CODE (from Cilk.hs) -- TODO: GENERATE AUTOMATICALLY.

{-# LANGUAGE TypeFamilies, CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The entrypoint to an Accelerate backend based on generating sequential C code.
module Data.Array.Accelerate.C (run, CBackend, mkCBackend) where

import qualified Data.ByteString.Lazy as B
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Array.Accelerate (Acc)
import qualified Data.Array.Accelerate.AST as AST
import qualified Data.Array.Accelerate.Array.Sugar as Sug
import qualified Data.Array.Accelerate.Cilk.JITRuntime as J
import           Data.Array.Accelerate.Shared.EmitC (ParMode(..))

import qualified Data.Array.Accelerate.BackendKit.IRs.SimpleAcc   as S
import qualified Data.Array.Accelerate.BackendKit.SimpleArray     as SA
import           Data.Array.Accelerate.BackendClass
import           Data.Array.Accelerate.BackendKit.CompilerPipeline
                  (phase0, phase1, phase2, repackAcc, unpackArray, Phantom(..))

--------------------------------------------------------------------------------

#if 0 
-- Standard boilerplate for lifting a `Backend` instance into a regular `run` function.
run :: forall a . (Sug.Arrays a) => Acc a -> a
run acc = unsafePerformIO $ do
           remt <- (runRaw CBackend (phase0 acc) Nothing)
           copyToHost CBackend remt
#else 
-- Alternatively we can lift up from the `SimpleBackend` interface:
run :: forall a . (Sug.Arrays a) => Acc a -> a
run acc = unsafePerformIO $ do
           remts <- (simpleRunRaw CBackend (phase1$ phase0 acc) Nothing)
           arrs  <- mapM (simpleCopyToHost CBackend) remts
           return (repackAcc (undefined :: Acc a) arrs)
#endif


----------------------------------------------------------------------------------------------------
-- The main purpose of this file is to define a new Backend type

-- | This is an abstract type representing the internal state of the backend.
data CBackend = CBackend
  deriving (Show)

-- Nothing to do here but there could be initialization work in the future.
mkCBackend :: IO CBackend
mkCBackend = return CBackend
-- Likewise there could be configuration parameters:
-- data CConfig = CConfig {}
-- defaultCConfig = ...


-- FIXME: This should really be left in the ForeignPtr state and should only come
-- back to Haskell when the copyToHost is performed...
newtype CRemote a = CRemote [SA.AccArray]

-- Nothing here at the moment, needs to cache the file:
data CBlob a = CBlob 

-- | Create a new C backend based on the configuration information.

-- | C data is not really very "remote", it just lives on the C heap.
-- newtype CRemote a = ForeignPtr a 
instance Backend CBackend where
  type Remote CBackend a = CRemote a
  type Blob CBackend a = CBlob a 

  compile _ path acc = error "CBackend: separate compile stage not implemented."
--    return (InMemory path (return$ B.empty))

  compileFun1 = error "CBackend: compileFun not implemented yet."

  runRaw _ acc _blob =
    do arrs <- J.rawRunIO Sequential "" (phase2$ phase1 acc)
       return$ CRemote arrs

--  runFun = error "CBackend: runFun not implemented yet."

  copyToHost = hostCopy
  copyToDevice _b accA = deviceCopy accA
  copyToPeer _ x = return x

  -- No waiting to be done!
  waitRemote _rem = return ()
  useRemote _ rem = useRem rem
  
  separateMemorySpace _ = False
--  compilesToDisk _ = True

-- For now copying just means repacking
hostCopy :: (Sug.Arrays a) => CBackend -> CRemote a -> IO a
hostCopy _ (CRemote arrays) =
  return$
    repackAcc (undefined :: Acc a) arrays


deviceCopy :: forall a . (Sug.Arrays a) => a -> IO (Remote CBackend a)
deviceCopy acc = do
  let repr :: Sug.ArrRepr a
      repr = Sug.fromArr acc
  -- FIXME: Seems like unpackArray can't really handle an array of tuples.
  let (_,arr,_::Phantom a) = unpackArray repr
      res :: Remote CBackend a
      res = CRemote [arr]
  return res

useRem :: forall a . (Sug.Arrays a) => Remote CBackend a -> IO (AST.Acc a)
useRem rem@(CRemote arrays) =
--  return (AST.Use$ repackAcc (undefined :: Acc a) arrays)
  error "useRemote unfinished for CBackend instance"


--------------------------------------------------------------------------------

-- | An instance for the less-typed AST backend interface.
instance SimpleBackend CBackend where
  type SimpleRemote CBackend = CRemote SA.AccArray
  type SimpleBlob CBackend = CBlob ()
  
  -- simpleCompile
  -- simpleCompileFun1

  simpleRunRaw _ prog _blob =
    do arrs <- J.rawRunIO Sequential "" (phase2 prog)
       return$ [ CRemote [arr] | arr <- arrs ]

  -- simpleRunRawFun1

  -- These do EFFECTIVELY NOTHING for now:
  simpleCopyToHost _b (CRemote [arr]) = return arr
  simpleCopyToDevice _b arr = return (CRemote [arr])
  simpleCopyToPeer _ x = return x

  simpleUseRemote _ (CRemote [arr]) = return (S.Use arr)
  simpleWaitRemote _ = return () -- already copied!
  simpleSeparateMemorySpace _ = False

