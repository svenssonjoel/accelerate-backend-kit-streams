{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


-- | A pass to lower the `Generate` form into `NewArray` + `Kernel`.

module Data.Array.Accelerate.BackendKit.Phase3.DesugarGenerate (desugarGenerate) where

import           Data.Array.Accelerate.BackendKit.IRs.SimpleAcc
                  (Const(..), Type(..), Prim(..), NumPrim(..), ScalarPrim(..), Var)
import           Data.Array.Accelerate.BackendKit.IRs.GPUIR         as G
import           Data.Array.Accelerate.BackendKit.IRs.Metadata  (ArraySizeEstimate(..), FreeVars(..))
import           Data.Array.Accelerate.BackendKit.Utils.Helpers (genUnique, genUniqueWith, GensymM, strideName, fragileZip)
import           Control.Monad.State.Strict (runState)
-- import qualified Data.Set as S

--------------------------------------------------------------------------------

-- | Desugar Generate and Fold into explicit Kernels and NewArrays.
--   In the process this pass does a form of closure conversion.  Free
--   variables inside `Generate`s are passed explicitly to Kernels.
-- 
--   The metadata becomes less meaningful after this pass.
--   `ArraySizeEstimate` for a Kernel is not well defined in general,
--   so it is expected to be `UnknownSize`.
desugarGenerate :: GPUProg (ArraySizeEstimate,FreeVars) -> GPUProg (ArraySizeEstimate,FreeVars)
desugarGenerate prog@GPUProg{progBinds, uniqueCounter} =
  prog {
    progBinds    = binds, 
    uniqueCounter= newCounter
  }
  where
    (binds,newCounter) = runState (doBinds prog progBinds) uniqueCounter


-- This procedure keeps around a "size map" from array values names to
-- their number of elements.
doBinds :: GPUProg (ArraySizeEstimate,FreeVars) ->
           [GPUProgBind (ArraySizeEstimate,FreeVars)] -> GensymM [GPUProgBind (ArraySizeEstimate,FreeVars)]
doBinds _ [] = return []
doBinds prog (pb@GPUProgBind { outarrs, evtid, evtdeps,
                               decor=(sz,FreeVars arrayOpFvs), op } : rest) = do  
  let deflt = do rst <- doBinds prog rest
                 return $ pb : rst
  case op of
     Use  _       -> deflt
     Cond _ _ _   -> deflt
     ScalarCode _ -> deflt
     Fold _ _ _ _ -> deflt
     Scan _ _ _ _ -> deflt
     NewArray _   -> deflt
     Kernel {}    -> deflt

     -- A Generate breaks down into a separate NewArray and Kernel:      
     -- Assumes trivial (duplicatable) els:
     Generate els (Lam iterargs bod) -> do
       -- Here is where we establish the protocol on additional kernel arguments.
       -- Kernels take their index argument(s) PLUS free vars:
       let iterVs = map (\ (v, _, TInt) -> v) iterargs
           iters  = fragileZip iterVs els

           -- After this transformation in this pass, the output variable itself becomes a "free var":
           freebinds' = outarr1 : corefreebinds
           
       newevt <- genUniqueWith "evtNew"
       rst <- doBinds prog rest
       return $ 
         GPUProgBind newevt [] outarrs (sz,FreeVars []) (NewArray (foldl mulI one els)) :
         GPUProgBind evtid (newevt:evtdeps) [] (UnknownSize,FreeVars [])
                     (Kernel iters (Lam freebinds' (doBod iterVs bod))
                                   (map (EVr . fst3) freebinds')) :
         rst 
 where
   [outarr1@(arrnm,_spc,_ty)] = outarrs -- Touch this and you make the one-output-array assumption!

   -- All the free variables must be explicitly passed to the kernel:
   corefreebinds = map
               -- We do a bit of digging here to find the type of each freevar:
               (\fv -> case lookupProgBind fv (progBinds prog) of 
                        Nothing -> error$"variable not in progbinds: "++show fv
                        Just (GPUProgBind {outarrs=arrVs}) ->
                          let (Just tyy) = lookup fv (map (\(a,_,b) ->(a,b)) arrVs) in
                          (fv, typeToAddrSpc tyy, tyy)
               )
               arrayOpFvs
               
   -- Convert a Generate body into a Kernel body.  Rather than
   -- returning a value, this writes it to an output arr.
   doBod [ixV] (ScalarBlock bnds results stmts) =
     case results of
       [outv] -> ScalarBlock bnds []
                 (stmts ++ [SArrSet arrnm (EVr ixV) (EVr outv)])
       ls -> error$"DesugarGenerate.hs/doBod: not handling multi-result Generates presently: "++show ls

   doBod indexVs _ = error$"DesugarGenerate.hs/doBod: handling only 1D Generates, not dimension "++show (length indexVs)


--------------------------------------------------------------------------------

typeToAddrSpc :: Type -> MemLocation
typeToAddrSpc (TArray _ _) = Global
typeToAddrSpc _            = Default

fst3 :: (t, t1, t2) -> t
fst3 (a,_,_) = a

one :: Exp
one  = EConst (I 1)