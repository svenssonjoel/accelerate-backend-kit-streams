{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{- | This module contains helpers that are specific to the
     "Vectorized CodeGen" class using Accelerate.

     Thus this module depends on "Data.Array.Accelerate.SimpleAST".
     It also depends on EasyEmit.
-}

module Data.Array.Accelerate.BackendKit.Utils.Helpers
       (          
         -- * Helpers that capture certain conventions that the compiler follows:
         strideName, mkIndTy, isTupleTy,
         GensymM, genUnique, genUniqueWith,
         
         -- * Other AST Helpers
         mkPrj, mapMAE, mapMAEWithEnv, mapMAEWithGEnv,

         -- * Helpers for constructing bits of AST syntax while incorporating small optimizations.
         addI, mulI, quotI, remI, maybeLet, maybeLetAllowETups, 
         -- TODO [2013.02.10] Move these ^^ to CompilerUtils.hs
         
         -- * Miscellaneous
         fragileZip, fragileZip3, (#), 

         -- * Constants and functions for use in cost estimation:
         ifCost, derefCost, costPrim, costConst,
         defaultDupThreshold
         )
       where

import qualified Data.Map as M
import Data.Array.Accelerate.BackendKit.IRs.SimpleAcc  as S
import Text.PrettyPrint.HughesPJ as PP
import Foreign.Storable           (sizeOf)
import Prelude                    (error, ($), (.))
import Data.Int                   (Int)
import Data.Word                  (Word)
import Prelude                    ((++), show, return, Show)
import Control.Monad.State.Strict (State, get, put)
import Control.Applicative        ((<$>),(<*>),pure,Applicative)
import Debug.Trace                (trace)
import Prelude as P

----------------------------------------------------------------------------------------------------
-- Conventions:
----------------------------------------------------------------------------------------------------

-- | Given the name of an array variable resulting from a
--   non-segmented FOLD, what is the name of a scalar variable
--   containing its stride.
strideName :: Var -> Var
strideName avr = S.var (P.show avr P.++ "_foldstride")


-- | Types for N-dimensional indices are just tuples of ints.
mkIndTy ::Int -> Type
mkIndTy n = mkTTuple (P.take n (P.repeat TInt))

-- | Is the type a tuple type, including unit?
isTupleTy :: Type -> P.Bool
isTupleTy t@(TTuple [_]) = error$"isTupleTy: corrupt type found, singleton tuple: "++show t
isTupleTy (TTuple _) = P.True
isTupleTy _          = P.False


-- | A monad to use just for gensyms:
type GensymM = State Int 

-- | Generate a unique name
genUnique :: GensymM Var
genUnique = genUniqueWith "gensym_"

-- | Generate a unique name with user-provided "meaningful" prefix.
genUniqueWith :: P.String -> GensymM Var
genUniqueWith prefix =
  do cnt <- get
     put (cnt+1)
     return$ S.var$ prefix ++ show cnt


----------------------------------------------------------------------------------------------------
-- Costing:
----------------------------------------------------------------------------------------------------

-- | The cost of a conditional IN ADDITOON to the cost of the two branches.
ifCost :: Int
ifCost = 1

-- | The cost of looking up an element in an array.
derefCost :: Int
derefCost = 1

-- | For now we use a constant cost across all primitive operations.
costPrim :: Prim -> Int
costPrim _ = 1

-- | For now we assume that all constants are free:
costConst :: Const -> Int
costConst _ = 1 

-- | If the cost of computing a single element of an array is less
-- than or equal to this threshold, that array may be /recomputed/
-- freely rather than precomputing it in the form of the original
-- array.  In practice this is used to determine when to inline
-- `Generate` and `BackPermute` array operations into their downstream
-- consumers.
defaultDupThreshold :: Int
defaultDupThreshold = 5


-- | Irrespective of the exact cost, certain expressions are
--   considered trivial (and always duplicatable).
-- isTrivialE 

----------------------------------------------------------------------------------------------------
-- Other Helpers
----------------------------------------------------------------------------------------------------

-- | Safely make a projection, taking care not to project from a ONE
--   ELEMENT tuple (i.e. not a tuple):
mkPrj :: Int -> Int -> Int -> Exp -> Exp
mkPrj ind len total e | total P.<= 0 = 
  error$"mkPrj: something's wrong, total tuple size should not be "++show total++" expr: "++show (ETupProject ind len e)
mkPrj ind len total e | ind P.+ len P.> total = 
  error$"mkPrj: out of bounds tuple index, from tuple of (supposed) size "++show total++", expr:\n  "++show (ETupProject ind len e)  
mkPrj _ _ 1 e = e
mkPrj ind len _total (ETuple ls) = mkETuple$ reverse $ take len $ drop ind $ reverse ls
mkPrj ind len _total e = ETupProject ind len e 

-- Convenient integer operations
addI :: Exp -> Exp -> Exp
addI (EConst (I 0)) n = n
addI n (EConst (I 0)) = n
addI (EConst (I n)) (EConst (I m)) = EConst$ I$ n + m
addI n m              = EPrimApp TInt (NP Add) [n,m]

mulI :: Exp -> Exp -> Exp
mulI (EConst (I 0)) _ = EConst (I 0)
mulI _ (EConst (I 0)) = EConst (I 0)
mulI (EConst (I 1)) n = n
mulI n (EConst (I 1)) = n
mulI (EConst (I n)) (EConst (I m)) = EConst$ I$ n * m
mulI n m              = EPrimApp TInt (NP Mul) [n,m]

quotI :: Exp -> Exp -> Exp
quotI n (EConst (I 1)) = n
quotI (EConst (I n)) (EConst (I m)) = EConst$ I$ P.quot n m
quotI n m              = EPrimApp TInt (IP Quot) [n,m]

remI :: Exp -> Exp -> Exp
remI _ (EConst (I 1)) = EConst (I 0)
remI (EConst (I n)) (EConst (I m)) = EConst$ I$ P.rem n m
remI n m              = EPrimApp TInt (IP Rem) [n,m]

-- | Bind a let expression only if necessary.  Don't introduce
-- variable-variable copies.
maybeLet :: Exp -> Type -> (Var -> Exp) -> GensymM Exp
maybeLet ex ty dobod =
  case ex of
    EVr v -> return (dobod v)
    _ -> do tmp <- genUnique
            return (ELet (tmp,ty,ex) (dobod tmp))

-- | A variant which allows EITHER a variable or an ETuple expression without
-- introducing a let, but introduces a let otherwise.
maybeLetAllowETups :: Exp -> Type -> (Exp -> Exp) -> GensymM Exp
maybeLetAllowETups ex ty dobod =
  case ex of
    EVr v    -> return (dobod ex)
    ETuple v -> return (dobod ex)
    _ -> do tmp <- genUnique
            return (ELet (tmp,ty,ex) (dobod (EVr tmp)))


--------------------------------------------------------------------------------
-- Traversals

-- | Map a monadic function over all Exps contained in an AExp.
mapMAE :: Applicative f => (Exp -> f Exp) -> AExp -> f AExp
mapMAE fn = mapMAEWithEnv M.empty (\ _ e -> fn e) 

-- | A variant of `mapMAE` that also tracks the variable-type binding
--   environment and passes it to the client function.
mapMAEWithEnv :: Applicative f => M.Map Var Type -> (M.Map Var Type -> Exp -> f Exp) -> AExp -> f AExp
mapMAEWithEnv env fn = mapMAEWithGEnv env (\ _ ty -> ty) fn

-- | Generalized version of `mapMAEWithEnv` that doesn't specify what is stored in the
-- environment.  The user provides a function to lift bindings into
-- the value they desire.
mapMAEWithGEnv :: Applicative f => M.Map Var v ->        
                  (Var -> Type -> v) ->             -- Lift a new binding into the environment.
                  (M.Map Var v -> Exp -> f Exp) ->  -- The function being mapped
                  AExp -> f AExp
mapMAEWithGEnv env lift fn0 ae =
  case ae of
    Use _                    -> pure ae
    Vr _                     -> pure ae
    Cond a b c               -> Cond     <$> fn a <*> pure b <*> pure c
    Generate e lam1          -> Generate <$> fn e <*> doLam1 lam1
    Unit ex                  -> Unit     <$> fn ex
    Map      lam1 vr         -> Map      <$> doLam1 lam1 <*> pure vr
    ZipWith  lam2 v1 v2      -> ZipWith  <$> doLam2 lam2 <*> pure v1 <*> pure v2
    Backpermute ex lam1 vr   -> Backpermute <$> fn ex <*> doLam1 lam1 <*> pure vr
    Replicate template ex vr -> Replicate template <$> fn ex <*> pure vr
    Reshape   shE v          -> Reshape  <$> fn shE <*> pure v
    Index slc vr ex          -> Index slc vr <$> fn ex
    Fold     lam2 e v        -> Fold     <$> doLam2 lam2 <*> fn   e <*> pure v
    Fold1    lam2 v          -> Fold1    <$> doLam2 lam2 <*> pure v
    FoldSeg  lam2 e v w      -> FoldSeg  <$> doLam2 lam2 <*> fn   e <*> pure v <*> pure w
    Fold1Seg lam2 v w        -> Fold1Seg <$> doLam2 lam2 <*> pure v <*> pure w
    Scanl    lam2 e v        -> Scanl    <$> doLam2 lam2 <*> fn   e <*> pure v
    Scanl'   lam2 e v        -> Scanl'   <$> doLam2 lam2 <*> fn   e <*> pure v
    Scanl1   lam2   v        -> Scanl1   <$> doLam2 lam2 <*> pure v
    Scanr    lam2 e v        -> Scanr    <$> doLam2 lam2 <*> fn   e <*> pure v
    Scanr'   lam2 e v        -> Scanr'   <$> doLam2 lam2 <*> fn   e <*> pure v
    Scanr1   lam2   v        -> Scanr1   <$> doLam2 lam2 <*> pure v
    Stencil  lam1 b v        -> Stencil  <$> doLam1 lam1 <*> pure b <*> pure v
    Stencil2 lam2 b v c w    -> Stencil2 <$> doLam2 lam2 <*> pure b <*> pure v 
                                                         <*> pure c <*> pure w
    Permute lam2 v lam1 w    -> Permute <$> doLam2 lam2 <*> pure v
                                        <*> doLam1 lam1 <*> pure w
 where
   fn = fn0 env
   doLam1 (Lam1 (v,t) bod)       = Lam1 (v,t)       <$> fn0 (M.insert v (lift v t) env) bod
   doLam2 (Lam2 (v,t) (w,u) bod) = Lam2 (v,t) (w,u) <$> fn0 (M.insert v (lift v t) $
                                                             M.insert w (lift w u) env) bod   

traceFun :: (Show a, Show b) => String -> (a -> b) -> a -> b
traceFun msg fn =
  \ x ->
    let y = fn x in 
    trace (msg ++ " input: " ++ show x ++ " output: " ++ show y) y

-- | For debugging purposes we should really never use Data.Map.!  This is an
-- alternative with a better error message.
(#) :: (Ord a1, Show a, Show a1) => M.Map a1 a -> a1 -> a
mp # k = case M.lookup k mp of
          Nothing -> error$"Map.lookup: key "++show k++" is not in map:\n  "++show mp
          Just x  -> x

-- | Like `Prelude.zipWith`, but do not allow the lists to be different lengths.
-- Catches more bugs.
fragileZip :: (Show t1, Show t2) =>
              [t1] -> [t2] -> [(t1, t2)]
fragileZip a b = loop a b
  where
    loop [] []           = []
    loop (h1:t1) (h2:t2) = (h1,h2) : loop t1 t2
    loop _ _             = error$"fragileZip: lists were not the same length:\n  "++show a++"\n  "++show b

-- | This one mandates that all three lists be the same 
fragileZip3 :: [t] -> [t1] -> [t2] -> Maybe [(t, t1, t2)]
fragileZip3 a b c = loop a b c
  where
    loop [] [] []                = Just []
    loop (h1:t1) (h2:t2) (h3:t3) = ((h1,h2,h3) :) <$> loop t1 t2 t3
    loop _ _ _                   = Nothing