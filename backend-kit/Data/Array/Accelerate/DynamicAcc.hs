{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A library for the runtime construction of fully typed Accelerate programs.
--
-- This version attempts to directly construct the Accelerate.AST
-- (type-level De-bruijn index) representation.

module Data.Array.Accelerate.DynamicAcc
{-
       (-- * Dynamically typed AST pieces
         SealedExp, SealedAcc,
         
         -- * Runtime representation of Types and Constants:
         Type(..), Const(..),

         -- * Syntax-constructing functions
         constantD, useD, 
         unitD, mapD, 

         -- * Functions to convert `SimpleAcc` programs into fully-typed Accelerate
         --   programs.
         convertExp, convertClosedExp,

         t0, t1, t2  -- TEMP
       )
-}
       where

import           Data.Array.Accelerate as A hiding (Exp,(++))
import qualified Data.Array.Accelerate as A hiding (Exp, (++))
import qualified Data.Array.Accelerate.Type as T
import qualified Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as S
import           Data.Array.Accelerate.BackendKit.IRs.SimpleAcc
                 (Type(..), Const(..), AVar, Var, Prog(..))
import           Data.Array.Accelerate.BackendKit.Tests (allProgsMap, p1a, TestEntry(..))
import           Data.Array.Accelerate.BackendKit.SimpleArray (payloadsFromList1)
-- import Data.Array.Accelerate.Interpreter as I
-- import           Data.Array.Accelerate.BackendKit.IRs.Internal.AccClone (repackAcc)
import           Data.Array.Accelerate.BackendKit.Phase1.ToAccClone (repackAcc)
import qualified Data.Array.Accelerate.Array.Sugar as Sug
-- import qualified Data.Array.Accelerate.Array.Data  as Dat

import qualified Data.Array.Accelerate.AST                  as NAST
import           Data.Array.Accelerate.AST                  ( Idx(..) )

import Data.Array.Accelerate.BackendKit.Phase1.ToAccClone as Cvt (accToAccClone, expToExpClone)

import Data.Typeable (gcast)
import Data.Dynamic (Dynamic, fromDyn, fromDynamic, toDyn,
                     Typeable, Typeable3, mkTyConApp, TyCon, mkTyCon3, typeOf3, typeOf)
import Data.Map as M
import Prelude as P
import Data.Maybe (fromMaybe, fromJust)
-- import Data.Word
import Debug.Trace (trace)
-- import Control.Exception (bracket)
-- import Control.Monad (when)

--------------------------------------------------------------------------------
-- AST Representations
--------------------------------------------------------------------------------

-- TODO: make these pairs that keep around some printed rep for debugging purposes in
-- the case of a downcast error.  Also make them newtypes!
newtype SealedExp     = SealedExp     Dynamic deriving Show
newtype SealedOpenExp = SealedOpenExp Dynamic deriving Show
type SealedAcc = Dynamic

sealExp :: Typeable a => NAST.Exp () a -> SealedExp
sealExp = SealedExp . toDyn

sealAcc :: Typeable a => Acc a -> SealedAcc
sealAcc = toDyn

-- sealOpenExp :: (Typeable t1, t1 ~ NAST.OpenExp env aenv results) => t1 -> SealedOpenExp
-- sealOpenExp :: Typeable res => NAST.OpenExp env aenv res -> SealedOpenExp

sealOpenExp :: (Typeable env, Typeable aenv, Typeable results) =>
               NAST.OpenExp env aenv results -> SealedOpenExp
sealOpenExp (x :: NAST.OpenExp env aenv results) = SealedOpenExp (toDyn x)
  
-- Dynamic seems to succeed here whereas my "Sealed" approach + gcast did not:
downcastOE :: forall t1 env aenv results .
              (Typeable t1, t1 ~ NAST.OpenExp env aenv results, Typeable results) =>
              SealedOpenExp -> t1
downcastOE (SealedOpenExp dyn) =
  case fromDynamic dyn of
    Just (oe :: t1) -> oe
    Nothing ->
      error$"Attempt to unpack SealedOpenExp with type "++show dyn
      ++ ", expected type "++ show (toDyn (unused::t1))

downcastE :: forall a . Typeable a => SealedExp -> NAST.Exp () a
downcastE (SealedExp d) =
  case fromDynamic d of
    Just e -> e
    Nothing ->
      error$"Attempt to unpack SealedExp with type "++show d
         ++ ", expected type Exp "++ show (toDyn (unused::a))

downcastA :: forall a . Typeable a => SealedAcc -> Acc a
downcastA d = case fromDynamic d of
                Just e -> e
                Nothing ->
                  error$"Attempt to unpack SealedAcc with type "++show d
                     ++ ", expected type Acc "++ show (toDyn (unused::a))

toOpen :: SealedExp -> SealedOpenExp
toOpen (SealedExp dyn) =
#if 0   
  -- Make sure it casts appropriately:
  case fromDynamic dyn of
    -- No instance of Typeable elt:
    Just (oe :: NAST.OpenExp () aenv elt) -> SealedOpenExp (toDyn oe)
    Nothing ->
      error$"Attempt to unpack SealedExp with type "++show dyn
         ++ ", expected type Exp "++ show (toDyn (unused::a))
#else
  SealedOpenExp dyn
#endif
  

-- | Typed de-bruijn indices carry a full type-level environment and a cursor into
-- it.  This just seals such an index up as a monomorphic type.
data SealedIdx where
  SealedIdx :: (Typeable t, Typeable env) =>
               Idx env t -> SealedIdx
  -- Typeable env
  --  Elt t => 

--------------------------------------------------------------------------------
-- Type representations
--------------------------------------------------------------------------------                

-- | We enhance "Data.Array.Accelerate.Type.TupleType" with Elt constraints.
data EltTuple a where
  UnitTuple   ::                                               EltTuple ()
  SingleTuple :: Elt a          => T.ScalarType a           -> EltTuple a
  PairTuple   :: (Elt a, Elt b) => EltTuple a -> EltTuple b -> EltTuple (a, b)
 deriving Typeable

-- | This GADT allows monomorphic value to carry a type inside.
data SealedEltTuple where
  SealedEltTuple :: (Typeable a) =>
                    EltTuple a -> SealedEltTuple

-- | This is a bottle in which to store a type that satisfyies the Array class.
data SealedArrayType where
  -- Do we care about the ArrayElt class here?
  SealedArrayType :: Arrays a => Phantom a -> SealedArrayType

-- | Tuples of arrays rather than scalar `Elt`s.
data ArrTuple a where
  UnitTupleA   ::                                                     ArrTuple ()
  SingleTupleA :: Arrays a             => T.ScalarType a           -> ArrTuple a
  PairTupleA   :: (Arrays a, Arrays b) => ArrTuple a -> ArrTuple b -> ArrTuple (a, b)

-- TODO: CAN WE PHASE OUT SealedArrayType in favor of SealedArrTuple?
data SealedArrTuple where
  SealedArrTuple :: ArrTuple a -> SealedArrTuple

-- | Accelerate shape types, sealed up.
data SealedShapeType where
  -- Do we care about the ArrayElt class here?
  SealedShapeType :: Shape sh => Phantom sh -> SealedShapeType

-- | Just a simple signal that the value is not used, only the type.
data Phantom a = Phantom a deriving Show

-- | Dependent types!  Dynamically construct a type in a bottle.  It can be opened up
-- and used as a goal type when repacking array data or returning an Acc computation.
arrayTypeD :: Type -> SealedArrayType
arrayTypeD (TArray ndim elty) =
  case shapeTypeD ndim of
    SealedShapeType (_ :: Phantom sh) ->
     case elty of
       TInt   -> SealedArrayType (Phantom(unused:: Array sh Int))
       TFloat -> SealedArrayType (Phantom(unused:: Array sh Float))
arrayTypeD oth = error$"arrayTypeD: expected array type, got "++show oth

-- | Construct a Haskell type from an Int!  Why not?
shapeTypeD :: Int -> SealedShapeType
shapeTypeD 0 = SealedShapeType (Phantom Z)
shapeTypeD n =
  case shapeTypeD (n-1) of
    SealedShapeType (Phantom x :: Phantom sh) ->
      SealedShapeType (Phantom (x :. (unused::Int)))

-- | Convert the runtime, monomorphic type representation into a sealed container
-- with the true Haskell type inside.
scalarTypeD :: Type -> SealedEltTuple
scalarTypeD ty =
  case ty of
    TInt    -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Int)
    TInt8   -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Int8)
    TInt16  -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Int16)
    TInt32  -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Int32)
    TInt64  -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Int64)

    TWord   -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Word)
    TArray {} -> error$"scalarTypeD: expected scalar type, got "++show ty

data SealedFun -- ??

--------------------------------------------------------------------------------
-- AST Construction
--------------------------------------------------------------------------------

#if 0 
-- | Dynamically typed variant of `Data.Array.Accelerate.unit`.
unitD :: SealedEltTuple -> SealedExp -> SealedAcc
unitD elt exp =
  case elt of
    SealedEltTuple (t :: EltTuple et) ->
      case t of
        UnitTuple -> sealAcc$ unit$ constant ()
        SingleTuple (_ :: T.ScalarType s) ->
          sealAcc$ unit (downcastE exp :: NAST.Exp () s)
        PairTuple (_ :: EltTuple l) (_ :: EltTuple r) ->
          sealAcc$ unit (downcastE exp :: NAST.Exp () (l,r))

-- | Dynamically-typed variant of `Data.Array.Accelerate.use`.  However, this version
-- is less powerful, it only takes a single, logical array, not a tuple of arrays.
useD :: S.AccArray -> SealedAcc
useD arr =
  case sty of
    SealedArrayType (Phantom (_ :: aT)) ->
      sealAcc$ A.use$
      repackAcc (unused::Acc aT) [arr]
 where
   dty = S.accArrayToType arr
   sty = arrayTypeD dty
#endif

-- TODO: How to handle functions?
mapD :: SealedFun -> SealedAcc -> SealedAcc 
mapD = error "mapD"


constantOE :: Const -> SealedOpenExp
constantOE c = 
  case c of
    I   i -> let r :: Sug.EltRepr Int
                 r = ((),i)
                 y :: NAST.OpenExp () () Int
                 y = NAST.Const r
             in sealOpenExp y
    I8 i  -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Int8 )) :: NAST.OpenExp () () Int8)
    I16 i -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Int16)) :: NAST.OpenExp () () Int16)
    I32 i -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Int32)) :: NAST.OpenExp () () Int32)
    I64 i -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Int64)) :: NAST.OpenExp () () Int64)
    W8 i  -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Word8 )) :: NAST.OpenExp () () Word8)
    W16 i -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Word16)) :: NAST.OpenExp () () Word16)
    W32 i -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Word32)) :: NAST.OpenExp () () Word32)
    W64 i -> sealOpenExp ((NAST.Const (((),i) :: Sug.EltRepr Word64)) :: NAST.OpenExp () () Word64)
    B   b -> sealOpenExp ((NAST.Const (((),b) :: Sug.EltRepr Bool)) :: NAST.OpenExp () () Bool)

  

--------------------------------------------------------------------------------
-- TODO: These conversion functions could move to their own module:
--------------------------------------------------------------------------------

-- | Track the scalar, array environment, and combined, fast-access environment.
data EnvPack = EnvPack [(Var,Type)] [(AVar,Type)] (M.Map Var Type) 

emptyEnvPack :: EnvPack 
emptyEnvPack = EnvPack [] [] M.empty 

-- | New array binding
extendA :: AVar -> Type -> EnvPack -> EnvPack 
extendA avr ty (EnvPack eS eA mp) = EnvPack eS ((avr,ty):eA) (M.insert avr ty mp)

extendE :: Var -> Type -> EnvPack -> EnvPack 
extendE vr ty (EnvPack eS eA mp) = EnvPack ((vr,ty):eS) eA (M.insert vr ty mp)

type AENV0 = ()

-- convertExp :: S.Exp -> (forall a . Elt a => Exp a)
-- | Convert an entire `SimpleAcc` expression into a fully-typed (but sealed) Accelerate one.
--   Requires a type environments for the (open) `SimpleAcc` expression:    
--   one for free expression variables, one for free array variables.
--     
convertExp :: 
              EnvPack -> SealedLayout -> S.Exp -> SealedOpenExp
-- TODO: Add a second SealedLayout for array bindings.
convertExp ep@(EnvPack envE envA mp)
           slayout@(SealedLayout (lyt :: Layout env0 env0')) ex =
  trace("Converting exp "++show ex++" with layout "++show lyt++" and dyn env "++show (envE,envA))$
  let cE = convertExp ep slayout in 
  case ex of
    S.EConst c -> constantOE c
    -- This is tricky, because it needs to become a deBruin index ultimately...
    -- Or we need to stay at the level of HOAS...
    S.EVr vr -> -- Scalar (not array) variable.
      let (ind,ety) = lookupInd vr envE in
      case scalarTypeD ety of 
        selt@(SealedEltTuple (t :: EltTuple elt)) ->
           case t of
             SingleTuple (_ :: T.ScalarType sT) ->
               -- We know (Elt sT)...
               case prjEltTuple selt ind slayout of                 
                 SealedIdx (idx :: Idx env2 res) ->
                   -- Need to unify res and sT... might fail.
                   case gcast idx of
                     Just (idx' :: Idx env2 sT) -> 
                       let oe :: NAST.OpenExp env2 () elt
                           oe = NAST.Var idx' in
                       -- Need: Typeable3 (NAST.PreOpenExp NAST.OpenAcc)
                       -- Need: Typeable env1
                       sealOpenExp oe
             -- TODO: other cases
                     
    S.ELet (vr,ty,rhs) bod ->
      let rhs' = cE rhs
          ep'@(EnvPack _ _ m2) = extendE vr ty ep
          resTy = scalarTypeD (S.recoverExpType m2 bod)
          sty   = scalarTypeD ty
      in
       case sty of
        SealedEltTuple (trhs :: EltTuple rhs_elt) ->
         let
          -- To extend the env first we bump debruijn indices...
            bumped :: Layout (env0, rhs_elt) env0'
            bumped = incLayout lyt
            -- ... then we push the zero index in:
            newLayout :: Layout (env0, rhs_elt) (env0',rhs_elt)
            newLayout = bumped `PushLayout` ZeroIdx

            bod'  = convertExp ep' (SealedLayout newLayout) bod
         in
          --          SealedLayout (lyt :: Layout env2 env2'),
         case (resTy) of
         (SealedEltTuple (tres :: EltTuple res_elt)) ->
           case (trhs,tres) of
             (SingleTuple _, SingleTuple _) -> 
               let bod'' = (downcastOE bod') :: NAST.OpenExp (env0,rhs_elt) AENV0 res_elt
                   rhs'' = (downcastOE rhs') :: NAST.OpenExp env0 AENV0 rhs_elt
                   oe :: NAST.OpenExp env0 AENV0 res_elt
                   oe = NAST.Let rhs'' bod''
               in
               trace ("Incremented "++show slayout++" to "++show newLayout)
               sealOpenExp oe

    S.ECond e1 e2 e3 ->
      let d1 = cE e1
          d2 = cE e2
          d3 = cE e3
          ty = S.recoverExpType mp e2
      in case scalarTypeD ty of
          SealedEltTuple (t :: EltTuple elt) ->
           -- #define a macro for this?
           case t of
             UnitTuple -> 
               sealOpenExp((NAST.Cond
                            (downcastOE d1::NAST.Exp () Bool) 
                            (downcastOE d2::NAST.Exp () elt)
                            (downcastOE d3::NAST.Exp () elt))::NAST.Exp () elt)
             SingleTuple _ ->
               sealOpenExp((NAST.Cond
                            (downcastOE d1::NAST.Exp () Bool) 
                            (downcastOE d2::NAST.Exp () elt)
                            (downcastOE d3::NAST.Exp () elt))::NAST.Exp () elt)
             PairTuple _ _ ->
               sealOpenExp((NAST.Cond
                            (downcastOE d1::NAST.Exp () Bool) 
                            (downcastOE d2::NAST.Exp () elt)
                            (downcastOE d3::NAST.Exp () elt))::NAST.Exp () elt)

    ex -> error$ "convertExp: expression not handled yet "++show ex
  where
    lookupInd :: Var -> [(Var,Type)] -> (Int,Type)
    lookupInd v [] = error$"convertExp: unbound variable: "++show v
    lookupInd v ((v2,x):tl)
      | v == v2   = (0,x)
      | otherwise = let (i,y) = lookupInd v tl
                    in (i+1,y)


-- | Convert a closed `SimpleAcc` expression (no free vars) into a fully-typed (but
-- sealed) Accelerate one.
convertClosedExp :: S.Exp -> SealedExp
convertClosedExp ex = SealedExp $ 
  case ex of
    S.EVr v -> error$"convertClosedExp: free variable found: "++show v

convertProg :: S.Prog a -> SealedAcc
convertProg S.Prog{progBinds,progResults} =
  error "convertProg"

{-
convertClosedAExp :: S.AExp -> SealedAcc
convertClosedAExp ae =
  case ae of
    S.Use arr -> useD arr
-}

-- -- | Scopes are conversions from an environment to a super-environment of that environment.
-- type Scope scopeEnv globalEnv = forall el.(NAST.Idx scopeEnv el -> NAST.Idx globalEnv el)

-- -- | This scope converts from the empty environment to itself. Note that there
-- -- are by definition no indices into this environment, but the identity function typechecks.
-- identityScope :: Scope env env
-- identityScope = id

-- -- | Pushing onto a scope means that an index of zero into the newly extended
-- -- subscope will result in an index of zero into the extended superscope, since they
-- -- now both have the same type at the top. A successor index has its sub-index scoped and
-- -- then successor applied, preserving the original scoping beneath the new top
-- -- of the environments
-- pushScope :: Scope scopeEnv globalEnv -> Scope (scopeEnv, t) (globalEnv, t)
-- pushScope scope idx = case idx of
--   ZeroIdx -> ZeroIdx
--   SuccIdx idx -> SuccIdx (scope idx)

-- -- | Popping from a scope removes the top type from the sub-environment,
-- -- but preserves the super-environment. This is accomplished by applying
-- -- the original scope to the successor of the incoming index, which turns
-- -- an index into the original scope into an index into the popped scope.
-- popScope :: Scope (scopeEnv, t) globalEnv -> Scope scopeEnv globalEnv
-- popScope scope idx = scope (SuccIdx idx)


data SealedLayout where
  SealedLayout :: (Typeable env, Typeable env') =>
                  Layout env env' -> SealedLayout

instance Show SealedLayout where
  show (SealedLayout (lyt :: Layout env env')) = show lyt

instance Show (Layout env env') where
  show lyt = 
    case lyt of
      EmptyLayout -> "()"
      PushLayout lay2 idx -> "("++show lay2++", "++show idx++")"

-- | Project an element from a sealed layout.
prjSealed :: forall t . Typeable t => String -> Int -> SealedLayout -> (Phantom t, SealedIdx)
prjSealed str ix (SealedLayout (lyt :: Layout env env')) =
  (Phantom unused, SealedIdx x)
 where
   x :: Idx env t
   x = prjIdx str ix lyt

-- | Project an index to something of scalar-tuple type.
prjEltTuple :: SealedEltTuple -> Int -> SealedLayout -> SealedIdx
prjEltTuple (SealedEltTuple elt) ix slay =
  case elt of
    UnitTuple ->
      let (_::Phantom (),x) = prjSealed "" ix slay in x
    SingleTuple (_ :: T.ScalarType s) ->
      let (_::Phantom s,x) = prjSealed "" ix slay in x
    PairTuple (_ :: EltTuple l) (_ :: EltTuple r) ->
      let (_::Phantom (l,r),x) = prjSealed "" ix slay in x


emptySealedLayout :: SealedLayout 
emptySealedLayout = SealedLayout (EmptyLayout :: Layout () ())

-- Layouts (from Sharing.hs)
-- -------------------------

-- | A layout of an environment has an entry for each entry of the environment.
-- Each entry in the layout holds the de Bruijn index that refers to the
-- corresponding entry in the environment.
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t
              => Layout env env' -> Idx env t -> Layout env (env', t)

-- | Project the nth index out of an environment layout.
-- The first argument provides context information for error messages in the case of failure.
prjIdx :: forall t env env'. Typeable t => String -> Int -> Layout env env' -> Idx env t
prjIdx ctxt orign origl = loop ctxt orign origl
 where
  loop :: forall t env env'. Typeable t => String -> Int -> Layout env env' -> Idx env t   
  loop ctxt 0 (PushLayout _ (ix :: Idx env0 t0))
    = flip fromMaybe (gcast ix)
    $ possiblyNestedErr ctxt $
        "Couldn't match expected type `" ++ show (typeOf (unused::t)) ++
        "' with actual type `" ++ show (typeOf (unused::t0)) ++ "'" ++
        "\n  Type mismatch"
  loop ctxt n (PushLayout l _)  = loop ctxt (n - 1) l
  loop ctxt n EmptyLayout       = possiblyNestedErr ctxt ("Index too large for environment (by "
                                                          ++show (n+1)++", orig idx "++show orign++"):\n  "
                                                          ++ show origl)

possiblyNestedErr :: String -> String -> a
possiblyNestedErr ctxt failreason
  = error $ "Fatal error in prjIdx:"
      ++ "\n  " ++ failreason ++ " at " ++ ctxt
      ++ "\n  Possible reason: nested data parallelism — array computation that depends on a"
      ++ "\n    scalar variable of type 'Exp a'"

-- | Add an entry to a layout, incrementing all indices
incLayout :: Layout env env' -> Layout (env, t) env'
incLayout EmptyLayout         = EmptyLayout
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (SuccIdx ix)


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------    

instance Show (EltTuple a) where
  show UnitTuple = "()"
  show (SingleTuple st) = show st
  show (PairTuple a b)  = "("++show a++","++show b++")"

instance Show SealedEltTuple where
  show (SealedEltTuple x) = "Sealed:"++show x

instance Show SealedShapeType where
  show (SealedShapeType (Phantom (_ :: sh))) =
    "Sealed:"++show (toDyn (unused::sh))
    
instance Show SealedArrayType where
  show (SealedArrayType (Phantom (_ :: sh))) =
    "Sealed:"++show (toDyn (unused::sh))

instance Show SealedIdx where
  show (SealedIdx idx) = show idx

instance Show (Idx env t) where
  show idx = "idx:"++show (NAST.idxToInt idx)


-- Cheat sheet:
-- type OpenExp = PreOpenExp OpenAcc
-- type PreExp acc = PreOpenExp acc ()
-- type Exp = OpenExp ()
-- data PreOpenExp (acc :: * -> * -> *) env aenv t where

{-# NOINLINE openExpTyCon #-}
openExpTyCon :: TyCon
openExpTyCon = mkTyCon3 "accelerate-backend-kit" "Data.Array.Accelerate.DynamicAcc.OpenExp" "OpenExp"

-- instance Typeable3 (NAST.PreOpenExp NAST.OpenAcc) where
instance Typeable3 (NAST.OpenExp) where
  typeOf3 (_ :: NAST.OpenExp env aenv results) =
    mkTyConApp openExpTyCon []
       -- [typeOf (unused::env),
       --  typeOf (unused::aenv),
       --  typeOf (unused::results)]

-- instance Typeable a => Typeable (EltTuple a) where
-- instance Typeable (EltTuple a) where  

  
--------------------------------------------------------------------------------
-- Misc, Tests, and Examples
--------------------------------------------------------------------------------  

unused :: a
unused = error "This dummy value should not be used"

-- | For debugging purposes we should really never use Data.Map.!  This is an
-- alternative with a better error message.
(#) :: (Ord a1, Show a, Show a1) => M.Map a1 a -> a1 -> a
mp # k = case M.lookup k mp of
          Nothing -> error$"Map.lookup: key "++show k++" is not in map:\n  "++show mp
          Just x  -> x

c0 :: SealedOpenExp
c0 = constantOE (I 99)

c0a :: NAST.Exp () Int
c0a = downcastOE c0

{-
-- Small tests:
t0 :: SealedAcc
t0 = convertClosedAExp $
     S.Use (S.AccArray [5,2] (payloadsFromList1$ P.map I [1..10]))
t0b :: Acc (Array DIM2 (Int))
t0b = downcastA t0
-}

t1 = -- convertClosedExp
     convertExp emptyEnvPack emptySealedLayout
     (S.ECond (S.EConst (B True)) (S.EConst (I 33)) (S.EConst (I 34)))
t1b :: NAST.Exp () Int
t1b = downcastOE t1

t2 :: SealedOpenExp
t2 = convertExp emptyEnvPack emptySealedLayout
     (S.ELet (v, TInt, (S.EConst (I 33))) (S.EVr v))
 where v = S.var "v" 

-- t2a :: Exp Int
-- t2a = downcastE t2

-- t2b :: NAST.OpenExp ((),(EltTuple Int)) () Int
-- t2b :: NAST.OpenExp ((),Int) () Int
t2b :: NAST.OpenExp () () Int
t2b = downcastOE t2

t4 = simpleProg
 where
   TestEntry {simpleProg} = allProgsMap # "p1a"

