


module Experiment1 where


import System.Posix.DynamicLinker

import System.IO.Unsafe

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

import Data.Word

import System.Process
import Control.Concurrent

-- import Graphics.UI.WX 

{-
  RTLD FLAGS

   RTLD_NOW:

   RTLD_LAZY:

   RTLD_LOCAL:

   RTLD_GLOBAL:

-} 

type OpNode = Ptr ()

-- foreign import ccall "dynamic" 
--   mkHarness :: FunPtr (IO Int) -> IO Int

foreign import ccall "dynamic" 
  mkHarness :: FunPtr (Ptr () -> IO Int) -> Ptr () -> IO Int


-- foreign import ccall "dynamic" 
--   mkRepoint :: FunPtr (FunPtr (Int -> IO ()) -> IO ()) -> FunPtr (Int -> IO ()) -> IO ()

foreign import ccall "dynamic" 
  mkRepoint :: FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> Ptr ()  -> IO ()

foreign import ccall "dynamic" 
  mkReplace :: FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> Ptr ()  -> IO ()


foreign import ccall "dynamic" 
  mkVoidFun :: FunPtr (IO ()) -> IO ()

-- Really dont think i need this! (Which is good) 
foreign import ccall "dynamic"
  mkOp :: FunPtr (FunPtr (Int -> IO ()) -> Int -> IO ()) -> FunPtr (Int -> IO ()) -> Int -> IO ()




foreign import ccall "dynamic"
  mkMkNode :: FunPtr (FunPtr (Ptr () -> Int -> IO ()) -> Ptr () -> IO (Ptr ()))
              -> FunPtr (Ptr () -> Int -> IO ()) -> Ptr () -> IO (Ptr ())

foreign import ccall "dynamic"
  mkTerm :: FunPtr (IO OpNode) -> IO OpNode 


---------------------------------------------------------------------------
-- Experiment 1
---------------------------------------------------------------------------
experiment1 = do

  dl <- dlopen "harness.so" [RTLD_NOW]
  -- dlop <- dlopen "op1.so" [RTLD_NOW] 


  harness <- dlsym dl "harness"

  nodeMaker <- dlsym dl "mkNode" 
  let mkNode = mkMkNode nodeMaker

  terminator <- dlsym dl "termnode"
  term   <- mkTerm terminator
      
  halt    <- dlsym dl "halt"
  
  op1     <- dlsym dl "op1"
  op2     <- dlsym dl "op2"
  op3     <- dlsym dl "op3" 

  myOp <- mkNode op1 term
  myOp2 <- mkNode op2 term
  myOp3 <- mkNode op3 term

  repoint <- dlsym dl "repoint"
  replace <- dlsym dl "replace" 
 

  threadID <- forkOS $ do mkHarness harness myOp
                          return ()
  
  putStrLn "Waiting"                         
  threadDelay (10^6 * 3)

  putStrLn "hot Swapping Function"                         
  mkRepoint repoint myOp myOp2 
   
  -- putStrLn "Waiting"                         
  threadDelay (10^6 * 3)


  putStrLn "hot Swapping Function"                         
  mkReplace replace myOp2 myOp3 
  
  threadDelay (10^6 * 3)

  putStrLn "hot Swapping Function"
  -- does not work, because the previous replace
  -- destroyed myOp2
  mkReplace replace myOp3 myOp2 
  
  threadDelay (10^6 * 3)
  
  putStrLn "Killing"
  
  mkVoidFun halt
  --killThread threadID 


  -- pval <- peek (castFunPtrToPtr op1 :: Ptr CUIntPtr ) -- something of same size as a funptr 
  -- poke (castFunPtrToPtr sink) pval
