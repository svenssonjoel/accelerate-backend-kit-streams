{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

--------------------------------------------------------------------------------
-- | Compiler pass to lift Lets in array expressions
--------------------------------------------------------------------------------

module Data.Array.Accelerate.SimplePasses.LiftLets
       ( liftLets, gatherLets )
       where 

-- standard libraries
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Prelude                                     hiding (sum)
import Control.Monad.State.Strict (State, runState, get, put)
import Data.List as L
import Data.Array.Accelerate.SimpleAST   as S
import Data.Array.Accelerate.SimplePasses.IRTypes as T

import Debug.Trace(trace)

-- Shorthands:
type TAExp = T.AExp S.Type
type Binding  a = (S.Var, S.Type, a)
type Bindings a = [Binding a]

--------------------------------------------------------------------------------

-- | This pass lifts all Let bindings to the outside.  
-- 
--   @Apply@ also gets converted to @Let@ in this pass.
-- 
--   The resulting program will have a spine of Lets (with Let-free
--   RHS's) followed by a Let-free body.
liftLets :: T.AExp S.Type -> T.AExp S.Type
liftLets x = 
     if L.null binds then loop binds else
     trace ("[dbg] Lifted out "++show (length binds)++" Lets ...") $ loop binds
  where (binds, bod) = gatherLets x
        finalTy = getAnnot bod
        loop [] = bod
        loop (hd:tl) = T.Let finalTy hd $ loop tl


-- | This is an alternative version of `liftLets` that extracts out
--   the let bindings but does not reinsert them as outer Let
--   expressions.  Rather, it returns the lifted bindings directly.
gatherLets :: TAExp -> ([(S.Var, S.Type, TAExp)], TAExp)
gatherLets prog = (reverse binds, prog')
 where 
   (prog',binds) = runState (loop prog) [] 
   addbind bnd = do ls <- get; put (bnd:ls)
   loop :: TAExp -> State (Bindings TAExp) TAExp
   loop aex = 
     case aex of 
       T.Let _ (v,ty,rhs) bod -> 
          do rhs' <- loop rhs -- Important: Collect these bindings first.
             addbind (v,ty,rhs')
             loop bod
       -- We ALSO treat Apply's as Let bindings:
       T.Apply _ fn ae -> 
          do let S.Lam1 (v,ty) abod = fn 
             rhs' <- loop ae
             addbind (v,ty,rhs')
             loop abod
       -- The rest is BOILERPLATE:      
       ----------------------------------------      
       T.Vr _ _              -> return aex
       T.Unit _ _            -> return aex
       T.Use _ _             -> return aex
       T.Generate _ _ _      -> return aex
       T.ZipWith a fn ae1 ae2  -> T.ZipWith a fn <$> loop ae1 <*> loop ae2 
       T.Map     a fn ae       -> T.Map     a fn <$> loop ae
       T.TupleRefFromRight a ind ae -> T.TupleRefFromRight a ind <$> loop ae
       T.Cond a ex ae1 ae2     -> T.Cond a ex <$> loop ae1 <*> loop ae2 
       T.Replicate aty slice ex ae -> T.Replicate aty slice ex <$> loop ae
       T.Index     a slc ae ex -> (\ ae' -> T.Index a slc ae' ex) <$> loop ae
       T.Fold  a fn einit ae         -> T.Fold  a fn einit    <$> loop ae
       T.Fold1 a fn       ae         -> T.Fold1 a fn          <$> loop ae 
       T.FoldSeg a fn einit ae aeseg -> T.FoldSeg a fn einit  <$> loop ae <*> loop aeseg 
       T.Fold1Seg a fn      ae aeseg -> T.Fold1Seg a fn       <$> loop ae <*> loop aeseg 
       T.Scanl    a fn einit ae      -> T.Scanl    a fn einit <$> loop ae  
       T.Scanl'   a fn einit ae      -> T.Scanl'   a fn einit <$> loop ae  
       T.Scanl1   a fn       ae      -> T.Scanl1   a fn       <$> loop ae 
       T.Scanr    a fn einit ae      -> T.Scanr    a fn einit <$> loop ae 
       T.Scanr'   a fn einit ae      -> T.Scanr'   a fn einit <$> loop ae 
       T.Scanr1   a fn       ae      -> T.Scanr1   a fn       <$> loop ae
       T.Permute a fn2 ae1 fn1 ae2 -> (\ x y -> T.Permute a fn2 x fn1 y)
                                 <$> loop ae1 <*> loop ae2
       T.Backpermute a ex lam ae -> T.Backpermute a ex lam   <$> loop ae
       T.Reshape     a ex     ae -> T.Reshape     a ex       <$> loop ae
       T.Stencil   a fn bndry ae -> T.Stencil     a fn bndry <$> loop ae
       T.Stencil2  a fn bnd1 ae1 bnd2 ae2 -> (\ x y -> T.Stencil2 a fn bnd1 x bnd2 y) 
                                        <$> loop ae1 <*> loop ae2
       T.ArrayTuple a aes -> T.ArrayTuple a <$> mapM loop aes


