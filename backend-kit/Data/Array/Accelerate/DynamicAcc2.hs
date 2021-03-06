{-# LANGUAGE RankNTypes, ScopedTypeVariables, GADTs #-} 
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | A library for the runtime construction of fully typed Accelerate programs.
-- 
-- In contrast with DynamicAcc.hs, this version uses the "Smart"
-- (HOAS) AST representation, rather than going straight to type-level
-- De-bruijn indices.
--
-- TODO: If we want to use this seriously, then switch to template
-- Haskell or some other, better boilerplate elimination mechanism,
-- not CPP macros.
--
-- TODO: We should simply TYPECHECK SimpleAcc expressions to get nice
-- error messages before putting them into the dynamic casting meat
-- grinder below.

module Data.Array.Accelerate.DynamicAcc2
       (

         -- * Functions to convert `SimpleAcc` programs into fully-typed Accelerate
         --   programs.
         convertProg, convertAcc, convertExp, 

         -- * Dynamically typed AST pieces
         SealedExp, SealedAcc,
         downcastE, downcastA,

         -- * Computing types at runtime so as to downcast:
         scalarTypeD, SealedEltTuple(..), 
         shapeTypeD,  SealedShapeType(..), 
         arrayTypeD,  SealedArrayType(..),         

         -- * Operating on open array and scalar expressions:
         convertOpenAcc, convertOpenExp,
         emptyEnvPack, extendA, extendE, 
         
         -- * REEXPORTs: for convenience
         Type(..), Const(..), Phantom(..),

         -- * INTERNAL: Syntax-constructing functions, operating over
         -- `Sealed`, dynamic representations.
         constantE, useD, unitD, mapD, generateD, foldD, dbgtrace
       )
       where

import           Data.Array.Accelerate as A hiding ((++))
import qualified Data.Array.Accelerate.Smart as Sm
import qualified Data.Array.Accelerate.Type as T
import qualified Data.Array.Accelerate.Trafo as Trafo
-- import           Data.Array.Accelerate.Array.Representation (SliceIndex(..))
import qualified Data.Array.Accelerate.Array.Representation as R
import qualified Data.Array.Accelerate.Array.Sugar as Sug
import qualified Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as S
import           Data.Array.Accelerate.BackendKit.IRs.SimpleAcc
                   (Type(..), Const(..), AVar, Var, Prog(..), 
                    Prim(..), NumPrim(..), IntPrim(..), FloatPrim(..), ScalarPrim(..))
import           Data.Array.Accelerate.BackendKit.Phase1.ToAccClone (repackAcc, expType, convertSliceIndex)
import           Data.Array.Accelerate.BackendKit.Utils.Helpers (Phantom(Phantom), maybtrace, dbg)

import           Data.Array.Accelerate.BackendKit.CompilerPipeline (phase0, phase1)

import Control.Exception (assert)
import Data.Bits as B
import Data.Dynamic (Typeable, Dynamic, fromDynamic, toDyn, typeOf)
import Data.Map as M
import Debug.Trace
import Prelude as P 
import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)

dbgtrace = 
  if dbg >= 5 
  then maybtrace
  else \ _s x -> x
  

------------------------------------------------------------------------------------------

-- INCOMPLETE: In several places we have an incomplete pattern match
-- due to no Elt instances for C types: [2013.08.09]
#define CTYERR error "DynamicAcc: Could not handle CType, it is not in Elt yet!"
#define INSERT_CTY_ERR_CASES \
  TCFloat  -> CTYERR; \
  TCDouble -> CTYERR; \
  TCShort  -> CTYERR; \
  TCInt    -> CTYERR; \
  TCLong   -> CTYERR; \
  TCLLong  -> CTYERR; \
  TCUShort  -> CTYERR; \
  TCUInt    -> CTYERR; \
  TCULong   -> CTYERR; \
  TCULLong  -> CTYERR; \
  TCChar    -> CTYERR; \
  TCSChar   -> CTYERR; \
  TCUChar   -> CTYERR; \

--------------------------------------------------------------------------------
-- AST Representations
--------------------------------------------------------------------------------

-- TODO: make these pairs that keep around some printed rep for debugging purposes in
-- the case of a downcast error.  Also make them newtypes!
-- 
-- TODO: add the S.Type itself to each of these.
data SealedExp     = SealedExp { expTy :: S.Type, expDyn :: Dynamic } deriving Show

data SealedAcc = SealedAcc { arrTy :: ArrTy, accDyn :: Dynamic } deriving Show

data ArrTy = ArrTy { ndims :: Int, eltTy :: S.Type } deriving Show

-- newtype SealedSlice = SealedSlice Dynamic deriving Show

sealExp :: (Elt a, Typeable a) => A.Exp a -> SealedExp
sealExp x = SealedExp ety (toDyn x)
 where
  ety = expType (Trafo.convertExp x)

sealAcc :: (Arrays a, Typeable a) => Acc a -> SealedAcc
sealAcc x = 
  dbgtrace (" ** Creating arrTy: "++show ty0++" for "++show x) $
  SealedAcc (ArrTy dims elty) (toDyn x)
 where
  ty0@(TArray dims elty) = progType $ phase1 $ phase0 x

-- sealSlice :: (Typeable s) => s -> SealedSlice
-- sealSlice = SealedSlice . toDyn

-- | Cast a sealed expression into a statically typed one.  This may
-- fail with an exception.
downcastE :: forall a . Typeable a => SealedExp -> A.Exp a
downcastE (SealedExp _ d) =
  case fromDynamic d of
    Just e -> e
    Nothing ->
      error$"Attempt to unpack SealedExp "++show d
         ++ ", expecting type Exp "++ show (toDyn (unused::a))

unused :: a
unused = error "This dummy value should not be used"

-- | Cast a sealed array expression into a statically typed one.  This
-- may fail with an exception.
downcastA :: forall a . Typeable a => SealedAcc -> Acc a
downcastA (SealedAcc _ d) =
  case fromDynamic d of
    Just e -> e
    Nothing ->
       error$"Attempt to unpack SealedAcc "++show d
          ++ ", expecting type Acc "++ show (toDyn (unused::a))
-- TODO: could expose the Maybe here for the variants we export.


-- | Convert a `SimpleAcc` constant into a fully-typed (but sealed) Accelerate one.
constantE :: Const -> SealedExp

#define SEALIT(pat) pat x -> sealExp (A.constant x); 
#define ERRIT(pat) pat x -> error$"constantE: Accelerate is missing Elt instances for C types presently: "++show x;
constantE c =
  case c of {
    SEALIT(I) SEALIT(I8) SEALIT(I16) SEALIT(I32) SEALIT(I64)
    SEALIT(W) SEALIT(W8) SEALIT(W16) SEALIT(W32) SEALIT(W64)
    SEALIT(F) SEALIT(D)
    SEALIT(B)
    ERRIT(CS) ERRIT(CI) ERRIT(CL) ERRIT(CLL)
    ERRIT(CUS) ERRIT(CUI) ERRIT(CUL) ERRIT(CULL)
    ERRIT(C) ERRIT(CC) ERRIT(CSC) ERRIT(CUC)    
    ERRIT(CF) ERRIT(CD)
    Tup [] -> sealExp $ A.constant ();
--    Tup ls -> error$ "constantE: Cannot handle tuple constants!  These should be ETuple's: "++show c 
    Tup ls -> convertExp (S.ETuple$ P.map S.EConst ls)
  }

--------------------------------------------------------------------------------
-- Type representations
--------------------------------------------------------------------------------                

-- | We enhance "Data.Array.Accelerate.Type.TupleType" with Elt constraints.
-- 
--   Further, we attempt to model SURFACE tuples here, not their binary-tree encoding.
data EltTuple a where
  UnitTuple   ::                                               EltTuple ()
  SingleTuple :: (Elt a)        => T.ScalarType a           -> EltTuple a
  PairTuple   :: (Elt a, Elt b) => EltTuple a -> EltTuple b -> EltTuple (a, b)
  ThreeTuple  :: (Elt a, Elt b, Elt c) => EltTuple a -> EltTuple b -> EltTuple c -> EltTuple (a, b, c)
 deriving Typeable
-- TODO: ^^ Get rid of SingleTuple and possible just use the NilTup/SnocTup rep.

-- | This GADT allows monomorphic value to carry a type inside.
data SealedEltTuple where
  SealedEltTuple :: (Typeable a, Elt a) =>
                    EltTuple a -> SealedEltTuple

-- | This is a bottle in which to store a type that satisfyies the Array class.
data SealedArrayType where
  -- Do we care about the ArrayElt class here?
  SealedArrayType :: Arrays a => Phantom a -> SealedArrayType

-- | Tuples of arrays rather than scalar `Elt`s.
data ArrTuple a where
  UnitTupleA   ::                                                     ArrTuple ()
  SingleTupleA :: Arrays a             => Phantom a                -> ArrTuple a
  PairTupleA   :: (Arrays a, Arrays b) => ArrTuple a -> ArrTuple b -> ArrTuple (a, b)

-- TODO: CAN WE PHASE OUT SealedArrayType in favor of SealedArrTuple?
data SealedArrTuple where
  SealedArrTuple :: ArrTuple a -> SealedArrTuple

-- | Accelerate shape types, sealed up.
data SealedShapeType where
  -- Do we care about the ArrayElt class here?
  SealedShapeType :: Shape sh => Phantom sh -> SealedShapeType

data SealedSliceType where
  SealedSliceType :: (Sug.Slice s, Elt s) => Phantom s -> SealedSliceType
  deriving Typeable
  
-- data SliceIndex ix slice coSlice sliceDim where
--   SliceNil   :: SliceIndex () () () ()
--   SliceAll   ::
--    SliceIndex ix slice co dim -> SliceIndex (ix, ()) (slice, Int) co (dim, Int)
--   SliceFixed ::
--    SliceIndex ix slice co dim -> SliceIndex (ix, Int) slice (co, Int) (dim, Int)


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
    TWord    -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Word)
    TWord8   -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Word8)
    TWord16  -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Word16)
    TWord32  -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Word32)
    TWord64  -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Word64)
    TFloat   -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Float)
    TDouble  -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Double)    
    TBool    -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Bool)
    TChar    -> SealedEltTuple$ SingleTuple (T.scalarType :: T.ScalarType Char)

    INSERT_CTY_ERR_CASES

    -- Here we have a problem... we've lost the surface tuple
    -- representation.... What canonical tuple representation do we use?
    TTuple []      -> SealedEltTuple UnitTuple
    TTuple [sing]  -> scalarTypeD sing
    TTuple [x,y]   -> 
        case (scalarTypeD x, scalarTypeD y) of
        (SealedEltTuple (et1 :: EltTuple a),
         SealedEltTuple (et2 :: EltTuple b)) ->
          SealedEltTuple$ PairTuple et1 et2

    TTuple [x,y,z]   -> 
        case (scalarTypeD x, scalarTypeD y, scalarTypeD z) of
        (SealedEltTuple (et1),
         SealedEltTuple (et2),
         SealedEltTuple (et3)) ->
          SealedEltTuple$ ThreeTuple et1 et2 et3

    TTuple (hd:tl) ->
      error ("scalarTypeD: unifinished: "++show ty) $ 
      case (scalarTypeD hd, scalarTypeD (TTuple tl)) of
        (SealedEltTuple (et1 :: EltTuple a),
         SealedEltTuple (et2 :: EltTuple b)) ->
          SealedEltTuple$ PairTuple et1 et2
    TArray {} -> error$"scalarTypeD: expected scalar type, got "++show ty

-- | Almost dependent types!  Dynamically construct a type in a bottle
-- that is isomorphic to an input value.  It can be opened up and used
-- as a goal type when repacking array data or returning an Acc
-- computation.
arrayTypeD :: Type -> SealedArrayType
-- TODO: Fix this to take ArrTy
arrayTypeD (TArray ndim elty) =
  case shapeTypeD ndim of
    SealedShapeType (_ :: Phantom sh) ->      
#define ATY(t1,t2) t1 -> SealedArrayType (Phantom :: Phantom (Array sh t2));
     case elty of {
       ATY(TInt,Int) ATY(TInt8,Int8) ATY(TInt16,Int16) ATY(TInt32,Int32) ATY(TInt64,Int64) 
       ATY(TWord,Word) ATY(TWord8,Word8) ATY(TWord16,Word16) ATY(TWord32,Word32) ATY(TWord64,Word64) 
       ATY(TFloat,Float) ATY(TDouble,Double)
       ATY(TChar,Char) ATY(TBool,Bool)

       INSERT_CTY_ERR_CASES

       TTuple ls -> (case scalarTypeD elty of 
                      SealedEltTuple (et :: EltTuple etty) -> 
                       SealedArrayType (Phantom:: Phantom(Array sh etty)));
       TArray _ _ -> error$"arrayTypeD: nested array type, not allowed in Accelerate: "++show(TArray ndim elty)
     }
arrayTypeD x@(TTuple _) = error$"arrayTypeD: does not handle tuples of arrays yet: "++show x
arrayTypeD oth = error$"arrayTypeD: expected array type, got "++show oth


arrayTypeD' (ArrTy d t) = arrayTypeD (TArray d t)

-- | Construct a Haskell type from an Int!  Why not?
shapeTypeD :: Int -> SealedShapeType
shapeTypeD 0 = SealedShapeType (Phantom :: Phantom Z)
shapeTypeD n | n < 0 = error "shapeTypeD: Cannot take a negative number!"
shapeTypeD n =
  case shapeTypeD (n-1) of
    SealedShapeType (Phantom :: Phantom sh) ->
      SealedShapeType (Phantom :: Phantom (sh :. Int))


-- | Dynamically construct an inhabitant of the Slice class.
sliceTypeD :: S.SliceType -> SealedSliceType
sliceTypeD [] = SealedSliceType (Phantom :: Phantom Z)
sliceTypeD (S.Fixed:rst) = 
  case sliceTypeD rst of 
    SealedSliceType (_ :: Phantom slc) -> 
      SealedSliceType (Phantom :: Phantom (slc :. Int))
sliceTypeD (S.All:rst) = 
  case sliceTypeD rst of 
    SealedSliceType (_ :: Phantom slc) -> 
      SealedSliceType (Phantom :: Phantom (slc :. All))

--------------------------------------------------------------------------------
-- AST Construction
--------------------------------------------------------------------------------


-- | Dynamically typed variant of `Data.Array.Accelerate.unit`.
unitD :: SealedEltTuple -> SealedExp -> SealedAcc
unitD elt exp =
  dbgtrace (" ** starting unitD: "++show (elt,exp)) $
  case elt of
    SealedEltTuple (t :: EltTuple et) ->
      case t of
        UnitTuple -> sealAcc$ unit$ constant ()
        SingleTuple (_ :: T.ScalarType s) ->
          sealAcc$ unit (downcastE exp :: A.Exp  s)
        PairTuple (_ :: EltTuple l) (_ :: EltTuple r) ->
          sealAcc$ unit (downcastE exp :: A.Exp  (l,r))
        ThreeTuple (_ :: EltTuple a) (_ :: EltTuple b) (_ :: EltTuple c) ->
          sealAcc$ unit (downcastE exp :: A.Exp  (a,b,c))

-- | Dynamically-typed variant of `Data.Array.Accelerate.use`.  However, this version
-- is less powerful, it only takes a single, logical array, not a tuple of arrays.
useD :: S.AccArray -> SealedAcc
useD arr =
  case sty of
    SealedArrayType (Phantom :: Phantom aT) ->
      sealAcc$ A.use$
      repackAcc (unused::Acc aT) [arr]
 where
   dty = S.accArrayToType arr
   sty = arrayTypeD dty

generateD :: SealedExp -> (SealedExp -> SealedExp) ->
        S.Type -> SealedAcc 
generateD indSealed bodfn outArrTy =
  dbgtrace (" ** starting generateD fun: outArrTy "++show (outArrTy)) $
      let (TArray dims outty) = outArrTy in
       case (shapeTypeD dims, scalarTypeD outty) of
         (SealedShapeType (_ :: Phantom shp), 
          SealedEltTuple (outET :: EltTuple outT)) ->           
          let
            rawfn :: Exp shp -> Exp outT
            rawfn x =
              dbgtrace (" ** Inside generate scalar fun, downcasting bod "++
                     show (bodfn (sealExp x))++" to "++ show (typeOf (undefined::outT))) $
              downcastE (bodfn (sealExp x))
            dimE :: Exp shp
            dimE = dbgtrace (" ** Generate: downcasting dim "++show indSealed++" Expecting Z-based index of dims "++show dims) $
                   downcastE indSealed
            acc = A.generate dimE rawfn
          in
           dbgtrace (" ** .. Body of generateD: raw acc: "++show acc) $
           sealAcc acc


mapD :: (SealedExp -> SealedExp) -> SealedAcc -> S.Type -> SealedAcc 
mapD bodfn sealedInArr outElmTy = 
      let (ArrTy dims inty) = arrTy sealedInArr
          newAty = arrayTypeD (TArray dims outElmTy)
      in
       -- TODO: Do we really need outElmTy here?
       case (shapeTypeD dims, scalarTypeD inty, scalarTypeD outElmTy) of
         (SealedShapeType (_ :: Phantom shp), 
          SealedEltTuple (inET  :: EltTuple inT),
          SealedEltTuple (outET :: EltTuple outT)) ->
          let
            rawfn :: Exp inT -> Exp outT
            rawfn x = downcastE (bodfn (sealExp x))
            realIn :: Acc (Array shp inT)
            realIn = downcastA sealedInArr
          in
           -- Here we suffer PAIN to recover the Elt/Typeable instances:
           case (inET, outET) of
             (UnitTuple,     UnitTuple)     -> sealAcc $ A.map rawfn realIn
             (SingleTuple _, UnitTuple)     -> sealAcc $ A.map rawfn realIn
             (PairTuple _ _, UnitTuple)     -> sealAcc $ A.map rawfn realIn
             (ThreeTuple {}, UnitTuple)     -> sealAcc $ A.map rawfn realIn
             (UnitTuple,     SingleTuple _) -> sealAcc $ A.map rawfn realIn
             (SingleTuple _, SingleTuple _) -> sealAcc $ A.map rawfn realIn             
             (PairTuple _ _, SingleTuple _) -> sealAcc $ A.map rawfn realIn
             (ThreeTuple {}, SingleTuple _) -> sealAcc $ A.map rawfn realIn
             (UnitTuple,     PairTuple _ _) -> sealAcc $ A.map rawfn realIn
             (SingleTuple _, PairTuple _ _) -> sealAcc $ A.map rawfn realIn             
             (PairTuple _ _, PairTuple _ _) -> sealAcc $ A.map rawfn realIn
             (ThreeTuple {}, PairTuple _ _) -> sealAcc $ A.map rawfn realIn
             (UnitTuple,     ThreeTuple {}) -> sealAcc $ A.map rawfn realIn
             (SingleTuple _, ThreeTuple {}) -> sealAcc $ A.map rawfn realIn
             (PairTuple _ _, ThreeTuple {}) -> sealAcc $ A.map rawfn realIn
             (ThreeTuple {}, ThreeTuple {}) -> sealAcc $ A.map rawfn realIn

zipWithD :: (SealedExp -> SealedExp -> SealedExp) -> SealedAcc -> SealedAcc ->
            S.Type -> S.Type  -> S.Type -> SealedAcc 
zipWithD bodfn sealedInArr1 sealedInArr2 inArrTy1 inArrTy2 outElmTy = 
  error "FINISHME/DynamicAcc - zipWithD"


replicateD :: SealedSliceType -> SealedExp -> SealedAcc -> SealedAcc
replicateD slc exp arr = 
  case (slc) of  -- , scalarTypeD (expTy exp)
    (SealedSliceType (_::Phantom slc)) ->
--     SealedEltTuple (inET  :: EltTuple inT) ) -> 
     let e :: Exp slc
         e = error "FINISHME/DynamicAcc - replicateD"
--         _ = A.replicate e 
     in 
      error "FINISHME/DynamicAcc - replicateD"

foldD :: (SealedExp -> SealedExp -> SealedExp) -> SealedExp -> SealedAcc ->
         S.Type -> SealedAcc 
foldD bodfn initE sealedInArr inArrTy = 
      let (TArray dims inty) = inArrTy
          -- Knock off one dimension:
          newAty = arrayTypeD (TArray (dims - 1) inty)
      in
       case (shapeTypeD (dims - 1), scalarTypeD inty) of
         (SealedShapeType (_ :: Phantom shp), 
          SealedEltTuple (inET  :: EltTuple inT)) ->
           let
               rawfn :: Exp inT -> Exp inT -> Exp inT
               rawfn x y = downcastE (bodfn (sealExp x) (sealExp y))
               realIn :: Acc (Array (shp :. Int) inT)
               realIn = downcastA sealedInArr
               init :: Exp inT
               init = downcastE initE
           in
            case inET of
              (UnitTuple    )     -> sealAcc $ A.fold rawfn init realIn
              (SingleTuple _)     -> sealAcc $ A.fold rawfn init realIn
              (PairTuple _ _)     -> sealAcc $ A.fold rawfn init realIn
              (ThreeTuple _ _ _)  -> sealAcc $ A.fold rawfn init realIn
  

--------------------------------------------------------------------------------
-- TODO: These conversion functions could move to their own module:
--------------------------------------------------------------------------------

-- | Track the scalar, array environment, and combined, fast-access environment.
data EnvPack = EnvPack [(Var,Type)] [(AVar,Type)]
                 (M.Map Var (Type, Either SealedExp SealedAcc))
 deriving Show

expectEVar :: Either SealedExp SealedAcc -> SealedExp
expectEVar (Left se)  = se
expectEVar (Right sa) = error$"expected scalar variable, got variable bound to: "++show sa

expectAVar :: Either SealedExp SealedAcc -> SealedAcc
expectAVar (Left se)  = error$"expected array variable, got variable bound to: "++show se
expectAVar (Right sa) = sa

emptyEnvPack :: EnvPack 
emptyEnvPack = EnvPack [] [] M.empty 

-- | New array binding
extendA :: AVar -> Type -> SealedAcc -> EnvPack -> EnvPack 
extendA avr ty sld (EnvPack eS eA mp) =
  EnvPack eS ((avr,ty):eA)
          (M.insert avr (ty,Right sld) mp)

extendE :: Var -> Type -> SealedExp -> EnvPack -> EnvPack 
extendE vr ty sld (EnvPack eS eA mp) =
  EnvPack ((vr,ty):eS) eA
          (M.insert vr (ty,Left sld) mp)

type AENV0 = ()


resealTup [] = sealExp$ A.constant ()

resealTup [(_,sing)] = sing

resealTup [(SealedEltTuple (_ :: EltTuple aty), a'),
           (SealedEltTuple (_ :: EltTuple bty), b')] =
    sealExp$ Sm.tup2 (downcastE a' :: Exp aty,
                      downcastE b' :: Exp bty)

resealTup [(SealedEltTuple (_ :: EltTuple aty), a),
           (SealedEltTuple (_ :: EltTuple bty), b),
           (SealedEltTuple (_ :: EltTuple cty), c)] =
    sealExp$ Sm.tup3 (downcastE a :: Exp aty,
                      downcastE b :: Exp bty,
                      downcastE c :: Exp cty)

resealTup [(SealedEltTuple (_ :: EltTuple aty), a),
           (SealedEltTuple (_ :: EltTuple bty), b),
           (SealedEltTuple (_ :: EltTuple cty), c),
           (SealedEltTuple (_ :: EltTuple dty), d)] =
    sealExp$ Sm.tup4 (downcastE a :: Exp aty,
                      downcastE b :: Exp bty,
                      downcastE c :: Exp cty,
                      downcastE d :: Exp dty)

resealTup components =  
  error$ "resealTup: mismatched or unhandled tuple: "++show components

-- | Convert a closed expression.
convertExp :: S.Exp -> SealedExp
convertExp = convertOpenExp emptyEnvPack

-- | Convert an entire `SimpleAcc` expression into a fully-typed (but sealed) Accelerate one.
--   Requires a type environments for the (open) `SimpleAcc` expression:    
--   one for free expression variables, one for free array variables.
--     
convertOpenExp :: EnvPack -> S.Exp -> SealedExp
convertOpenExp ep@(EnvPack envE envA mp) ex =
  dbgtrace(" @ Converting exp "++show ex++" with env "++show mp++"\n    and dyn env "++show (envE,envA)) $
  dbgtrace(" @-> converted exp result: "++show result) $
  result
 where 
  cE = convertOpenExp ep
  typeEnv  = M.map P.fst mp -- Strip out the SealedExp/Acc bits leaving just the types.
  resultTy = S.recoverExpType typeEnv ex
  result =  
   case ex of
    S.EConst c -> constantE c
    S.EVr vr -> let (_,se) = mp # vr in expectEVar se

    S.EShape _          -> error "FINISHME/DynamicAcc: convertOpenExp needs to handle EShape"
    S.EShapeSize _      -> error "FINISHME/DynamicAcc: convertOpenExp needs to handle EShapeSize"
    S.EIndex _          -> error "FINISHME/DynamicAcc: convertOpenExp needs to handle EIndex"

    -- Here we run straight into our mismatch between Acc and
    -- SimpleAcc treatment of shape types.
    S.EIndexScalar avr indEx -> 
      let indTy = S.recoverExpType typeEnv indEx
          ind2  = tupToIndex indTy$ convertOpenExp ep indEx
          (arrty,sa) = mp # avr
          TArray dims elt = arrty
      in
       case (shapeTypeD dims, scalarTypeD elt) of
         (SealedShapeType (_ :: Phantom shp),
          SealedEltTuple  (_ :: EltTuple eT)) ->
           let ind3 :: Exp shp
               ind3 = downcastE ind2
               arr  :: Acc (Array shp eT)
               arr  = downcastA $ expectAVar sa
               exp  :: Exp eT
               exp  = (A.!) arr ind3
           in sealExp exp              
           
    -- FIXME: This is having a FLATTING effect, which isn't valid for surface tuples:
    S.ETupProject {S.indexFromRight=ind, S.projlen=len, S.tupexpr=tex} ->
      dbgtrace ("ETupProject: "++show(ind,len,tex)) $
      let tup = convertOpenExp ep tex
          tty  = S.recoverExpType typeEnv tex
      in       
       case scalarTypeD tty of
         SealedEltTuple (et1 :: EltTuple tupT) ->
           case et1 of
             UnitTuple -> error "Tup projection from unit."
             PairTuple (etA :: EltTuple aa) (etB@SingleTuple{} :: EltTuple bb) ->
               let (a,b) = unlift (downcastE tup :: Exp (aa,bb))
               in resealTup $ 
                P.take len $ P.drop ind $ P.zip [SealedEltTuple etA, SealedEltTuple etB]
                                                [sealExp a, sealExp b]

             -- PairTuple (ta :: EltTuple aa)
             --   (PairTuple (tb :: EltTuple bb)
             --    (tc@SingleTuple{} :: EltTuple cc)) ->
             ThreeTuple (ta :: EltTuple aa) (tb :: EltTuple bb) (tc :: EltTuple cc) ->
               let (a,b,c) = unlift (downcastE tup :: Exp (aa,bb,cc))
               in resealTup $ 
                P.take len $ P.drop ind $ P.zip [SealedEltTuple ta, SealedEltTuple tb, SealedEltTuple tc]
                                                [sealExp a, sealExp b, sealExp c]

             _ -> error ("ETupProject got unhandled tuple type: "++show et1)                
    
    S.ETuple []    -> constantE (Tup [])
    S.ETuple [ex]  -> convertOpenExp ep ex
    S.ETuple [a,b] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
      in
      resealTup $ P.zip [scalarTypeD ta, scalarTypeD tb] [a',b']

    -- <EXTREME PAIN>
    -------------------------------------------------------------------------------
    S.ETuple [a,b,c] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          tc = S.recoverExpType typeEnv c
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
          c' = convertOpenExp ep c          
      in
      resealTup $ P.zip (P.map scalarTypeD [ta, tb, tc]) [a',b',c']

    S.ETuple [a,b,c,d] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          tc = S.recoverExpType typeEnv c
          td = S.recoverExpType typeEnv d
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
          c' = convertOpenExp ep c
          d' = convertOpenExp ep d               
      in
       resealTup $ P.zip (P.map scalarTypeD [ta,tb,tc,td]) [a',b',c',d']

    S.ETuple [a,b,c,d,e] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          tc = S.recoverExpType typeEnv c
          td = S.recoverExpType typeEnv d
          te = S.recoverExpType typeEnv e          
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
          c' = convertOpenExp ep c
          d' = convertOpenExp ep d
          e' = convertOpenExp ep e          
      in
       resealTup $ P.zip (P.map scalarTypeD [ta,tb,tc,td,te]) [a',b',c',d',e']

    S.ETuple [a,b,c,d,e,f] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          tc = S.recoverExpType typeEnv c
          td = S.recoverExpType typeEnv d
          te = S.recoverExpType typeEnv e
          tf = S.recoverExpType typeEnv f
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
          c' = convertOpenExp ep c
          d' = convertOpenExp ep d
          e' = convertOpenExp ep e
          f' = convertOpenExp ep f
      in
       resealTup $ P.zip (P.map scalarTypeD [ta,tb,tc,td,te,tf]) [a',b',c',d',e',f']

    S.ETuple [a,b,c,d,e,f,g] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          tc = S.recoverExpType typeEnv c
          td = S.recoverExpType typeEnv d
          te = S.recoverExpType typeEnv e
          tf = S.recoverExpType typeEnv f
          tg = S.recoverExpType typeEnv g
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
          c' = convertOpenExp ep c
          d' = convertOpenExp ep d
          e' = convertOpenExp ep e
          f' = convertOpenExp ep f
          g' = convertOpenExp ep g
      in resealTup $ P.zip (P.map scalarTypeD [ta,tb,tc,td,te,tf,tg])
                           [a',b',c',d',e',f',g']

    S.ETuple [a,b,c,d,e,f,g,h] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          tc = S.recoverExpType typeEnv c
          td = S.recoverExpType typeEnv d
          te = S.recoverExpType typeEnv e
          tf = S.recoverExpType typeEnv f
          tg = S.recoverExpType typeEnv g
          th = S.recoverExpType typeEnv h
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
          c' = convertOpenExp ep c
          d' = convertOpenExp ep d
          e' = convertOpenExp ep e
          f' = convertOpenExp ep f
          g' = convertOpenExp ep g
          h' = convertOpenExp ep h
      in resealTup $ P.zip (P.map scalarTypeD [ta,tb,tc,td,te,tf,tg,th])
                           [a',b',c',d',e',f',g',h']

    S.ETuple [a,b,c,d,e,f,g,h,i] ->
      let ta = S.recoverExpType typeEnv a
          tb = S.recoverExpType typeEnv b
          tc = S.recoverExpType typeEnv c
          td = S.recoverExpType typeEnv d
          te = S.recoverExpType typeEnv e
          tf = S.recoverExpType typeEnv f
          tg = S.recoverExpType typeEnv g
          th = S.recoverExpType typeEnv h
          ti = S.recoverExpType typeEnv i
          a' = convertOpenExp ep a
          b' = convertOpenExp ep b
          c' = convertOpenExp ep c
          d' = convertOpenExp ep d
          e' = convertOpenExp ep e
          f' = convertOpenExp ep f
          g' = convertOpenExp ep g
          h' = convertOpenExp ep h
          i' = convertOpenExp ep i
      in resealTup $ P.zip (P.map scalarTypeD [ta,tb,tc,td,te,tf,tg,th,ti])
                           [a',b',c',d',e',f',g',h',i']

    S.ETuple (a:b:c:d:e:f:g:h:i:tl) ->
      error$"convertExp: Alas, tuples greater than size nine are not handled by Accelerate: "++
            show (a:b:c:d:e:f:g:h:i:tl)

    -------------------------------------------------------------------------------
    -- </EXTREME PAIN>

    -- Version 3: try to generalize tuple handling.  Failed, but leave it in a compiling state:
#if 0
    S.ETuple (hd:tl) ->
      let ta = S.recoverExpType typeEnv hd
          tb = S.recoverExpType typeEnv (S.ETuple tl)
          hd' = convertOpenExp ep hd
          tl' = convertOpenExp ep (S.ETuple tl)
      in 
      case (scalarTypeD ta, scalarTypeD tb) of
        (SealedEltTuple (et1 :: EltTuple aty),
         SealedEltTuple (et2 :: EltTuple bty)) ->
          case downcastE tl' :: Exp bty of
            Sm.Exp (Sm.Tuple (tupRst :: Tuple Exp (TupleRepr bty))) ->
             case tupRst of
               (_ :: Tuple Exp brep) ->
                let tup :: Tuple Exp (brep,aty)
                    tup = tupRst `SnocTup` (downcastE hd' :: Exp aty)
--                    tup' :: Sm.PreExp acc Exp (??? bty,aty ???)
--                    tup' = Sm.Tuple tup
                in
                error "FINISHME/DynamicAcc"    
                -- sealExp$ Sm.Exp tup'
#endif

    {- ========================================================================================== -}
    S.EPrimApp outTy op ls ->
      let args = P.map (convertOpenExp ep) ls 
          (fstArg :_) = ls
          fstArgTy = S.recoverExpType typeEnv fstArg
      in 
      -- Dispatch on the type of the first argument to the primitive:
      (case scalarTypeD fstArgTy of {
       SealedEltTuple t0 -> 
      (case t0 of {
       ThreeTuple _ _ _ -> error$ "Primitive "++show op++" should not have a tuple 1st argument type.";
       PairTuple _ _ -> error$ "Primitive "++show op++" should not have a tuple 1st argument type.";
       UnitTuple     -> error$ "Primitive "++show op++" should not have a unit 1st argument type.";
       SingleTuple (styIn1 :: T.ScalarType eltIn1) ->
      (case scalarTypeD outTy of 
        SealedEltTuple t ->
          case t of
            ThreeTuple _ _ _ -> error$ "Primitive "++show op++" should not have a tuple output type."
            PairTuple _ _ -> error$ "Primitive "++show op++" should not have a tuple output type."
            UnitTuple     -> error$ "Primitive "++show op++" should not have a unit output type."
            SingleTuple (styOut :: T.ScalarType elt) ->

-- <BOILERPLATE abstracted as CPP macros>
-----------------------------------------------------------------------------------              
#define REPBOP(numpat, popdict, which, theprim, binop) (numpat, which theprim) -> popdict (case args of { \
         [a1,a2] -> let a1',a2' :: Exp elt;    \
                        a1' = downcastE a1;     \
                        a2' = downcastE a2;     \
                    in sealExp (binop a1' a2'); \
         _ -> error$ "Binary operator "++show theprim++" expects two args, got "++show args ; })
#define REPUOP(numpat, popdict, which, prim, unop) (numpat, which prim) -> popdict (case args of { \
         [a1] -> let a1' :: Exp elt;     \
                     a1' = downcastE a1; \
                 in sealExp (unop a1');  \
         _ -> error$ "Unary operator "++show prim++" expects one arg, got "++show args ; })
-- These occur in a pattern context:
#define POPINT T.NumScalarType (T.IntegralNumType (nty :: T.IntegralType elt))
#define POPFLT T.NumScalarType (T.FloatingNumType (nty :: T.FloatingType elt))
#define POPIDICT case T.integralDict nty of (T.IntegralDict :: T.IntegralDict elt) ->
#define POPFDICT case T.floatingDict nty of (T.FloatingDict :: T.FloatingDict elt) ->
#define POPSDICT 
                                              
-- Monomorphic in second arg:
#define REPBOPMONO(numpat, popdict, which, prim, binop) (numpat, which prim) -> popdict (case args of { \
         [a1,a2] -> let a1' :: Exp elt;         \
                        a1' = downcastE a1;     \
                        a2' :: Exp Int;         \
                        a2' = downcastE a2;     \
                    in sealExp (binop a1' a2'); \
         _ -> error$ "Binary operator "++show prim++" expects two args, got "++show args ; })

#define REPUOP_I2F(prim, unop)  \
      (T.NumScalarType (T.IntegralNumType (ity :: T.IntegralType elt)), FP prim) -> \
        (case T.integralDict ity of { (T.IntegralDict :: T.IntegralDict elt) ->     \
         case styIn1 of { T.NumScalarType (T.FloatingNumType (fty :: T.FloatingType eltF)) -> \
         case T.floatingDict fty of { (T.FloatingDict :: T.FloatingDict eltF) -> \
         case args of { \
          [a1] -> (let a1' :: Exp eltF;    \
                       a1' = downcastE a1; \
                       res :: Exp elt;     \
                       res = unop a1';     \
                   in sealExp res);        \
          _ -> error$ "Unary operator "++show prim++" expects one arg, got "++show args ;};};};})

-----------------------------------------------------------------------------------
             (case (styOut,op) of 
               REPBOP(POPINT, POPIDICT, NP, Add, (+))
               REPBOP(POPINT, POPIDICT, NP, Sub, (-))
               REPBOP(POPINT, POPIDICT, NP, Mul, (*))
               REPUOP(POPINT, POPIDICT, NP, Abs, abs)
               REPUOP(POPINT, POPIDICT, NP, Neg, (\x -> (-x)))
               REPUOP(POPINT, POPIDICT, NP, Sig, signum)
               -- UGH, do the same thing with float dicts:
               REPBOP(POPFLT, POPFDICT, NP, Add, (+))
               REPBOP(POPFLT, POPFDICT, NP, Sub, (-))
               REPBOP(POPFLT, POPFDICT, NP, Mul, (*))
               REPUOP(POPFLT, POPFDICT, NP, Abs, abs)
               REPUOP(POPFLT, POPFDICT, NP, Neg, (\x -> (-x)))
               REPUOP(POPFLT, POPFDICT, NP, Sig, signum)

               REPBOP(POPINT, POPIDICT, IP, Quot, quot)
               REPBOP(POPINT, POPIDICT, IP, Rem,  rem)
               REPBOP(POPINT, POPIDICT, IP, IDiv, div)
               REPBOP(POPINT, POPIDICT, IP, Mod,  mod)
               REPBOP(POPINT, POPIDICT, IP, BAnd, (.&.))
               REPBOP(POPINT, POPIDICT, IP, BOr,  (.|.))
               REPBOP(POPINT, POPIDICT, IP, BXor, xor)
               REPUOP(POPINT, POPIDICT, IP, BNot, complement)
               -- These take monomorphic Int arguments:
               REPBOPMONO(POPINT, POPIDICT, IP, BShiftL, A.shiftL)
               REPBOPMONO(POPINT, POPIDICT, IP, BShiftR, A.shiftR)
               REPBOPMONO(POPINT, POPIDICT, IP, BRotateL, A.rotateL)
               REPBOPMONO(POPINT, POPIDICT, IP, BRotateR, A.rotateR)

               REPBOP(POPFLT, POPFDICT, FP, FDiv, (/))
               REPBOP(POPFLT, POPFDICT, FP, FPow, (**))
               REPBOP(POPFLT, POPFDICT, FP, LogBase, logBase)
               REPBOP(POPFLT, POPFDICT, FP, Atan2, atan2)
               REPUOP(POPFLT, POPFDICT, FP, Recip, recip)
               REPUOP(POPFLT, POPFDICT, FP, Sin, sin)
               REPUOP(POPFLT, POPFDICT, FP, Cos, cos)
               REPUOP(POPFLT, POPFDICT, FP, Tan, tan)
               REPUOP(POPFLT, POPFDICT, FP, Asin, asin)
               REPUOP(POPFLT, POPFDICT, FP, Acos, acos)
               REPUOP(POPFLT, POPFDICT, FP, Atan, atan)
               REPUOP(POPFLT, POPFDICT, FP, Asinh, asinh)
               REPUOP(POPFLT, POPFDICT, FP, Acosh, acosh)
               REPUOP(POPFLT, POPFDICT, FP, Atanh, atanh)
               REPUOP(POPFLT, POPFDICT, FP, ExpFloating, exp)
               REPUOP(POPFLT, POPFDICT, FP, Sqrt, sqrt)
               REPUOP(POPFLT, POPFDICT, FP, Log, log)

               -- Warning!  Heterogeneous input/output types:               
               REPUOP_I2F(Truncate, A.truncate)
               REPUOP_I2F(Round, A.round)
               REPUOP_I2F(Floor, A.floor)
               REPUOP_I2F(Ceiling, A.ceiling)
     
               -------------- Boolean Primitives --------------
               (_, BP S.And) -> (case args of { 
                 [a1,a2] -> let a1', a2' :: Exp Bool;
                                a1' = downcastE a1;     
                                a2' = downcastE a2;     
                            in sealExp (a1' A.&&* a2'); 
                 _ -> error$ "Boolean AND operator expects two args, got "++show args ; })

               (_, BP S.Or) -> (case args of { 
                 [a1,a2] -> let a1', a2' :: Exp Bool;
                                a1' = downcastE a1;     
                                a2' = downcastE a2;     
                            in sealExp (a1' A.||* a2'); 
                 _ -> error$ "Boolean OR operator expects two args, got "++show args ; })

               (_, BP S.Not) -> (case args of { 
                 [a1] -> let a1' :: Exp Bool;
                             a1' = downcastE a1;
                         in sealExp (A.not a1'); 
                 _ -> error$ "Boolean NOT operator expects one arg, got "++show args ; })


               -------------- Relational/Equality Primitives --------------

      -- (T.NumScalarType (T.IntegralNumType (ity :: T.IntegralType elt)), FP prim) -> \
      --   (case T.integralDict ity of { (T.IntegralDict :: T.IntegralDict elt) ->     \
      --    case styIn1 of { T.NumScalarType (T.FloatingNumType (fty :: T.FloatingType eltF)) -> \
      --    case T.floatingDict fty of { (T.FloatingDict :: T.FloatingDict eltF) -> \
      --    case args of { \
      --     [a1] -> (let a1' :: Exp eltF;    \
      --                  a1' = downcastE a1; \
      --                  res :: Exp elt;     \
      --                  res = unop a1';     \
      --              in sealExp res);        \
      --     _ -> error$ "Unary operator "++show prim++" expects one arg, got "++show args ;};};};})

               (_, SP S.Eq  ) -> (ordPrim styIn1 (A.==*) args)
               (_, SP S.NEq ) -> (ordPrim styIn1 (A./=*) args)
               (_, SP S.Gt  ) -> (ordPrim styIn1 (A.>*) args)
               (_, SP S.Lt  ) -> (ordPrim styIn1 (A.<*) args)
               (_, SP S.GtEq) -> (ordPrim styIn1 (A.>=*) args)
               (_, SP S.LtEq) -> (ordPrim styIn1 (A.<=*) args)
               -- FINSHME: Max & Min

               -------------- Other Primitives --------------

               -------------- Other Primitives --------------

               -- FromIntegral case 1/2: integral->integral
               (T.NumScalarType (T.IntegralNumType (ityOut :: T.IntegralType elt)), 
                OP S.FromIntegral) -> 
                (case T.integralDict ityOut of { (T.IntegralDict :: T.IntegralDict elt) -> 
                 case styIn1 of { T.NumScalarType (T.IntegralNumType (ity :: T.IntegralType inElt)) ->
                 case T.integralDict ity of { (T.IntegralDict :: T.IntegralDict inElt) -> 
                 case args of {
                 [a1] -> let a1' :: Exp inElt;
                             a1' = downcastE a1;
                             res :: Exp elt
                             res = A.fromIntegral a1'
                         in sealExp res;
                 _ -> error$ "fromIntegral operator expects one arg, got "++show args ;};};};})

               -- FromIntegral case 2/2: integral->floating
               (T.NumScalarType (T.FloatingNumType (ityOut :: T.FloatingType elt)), 
                OP S.FromIntegral) -> 
                (case T.floatingDict ityOut of { (T.FloatingDict :: T.FloatingDict eltF) -> 
                 case styIn1 of { T.NumScalarType (T.IntegralNumType (ity :: T.IntegralType inElt)) ->
                 case T.integralDict ity of { (T.IntegralDict :: T.IntegralDict inElt) -> 
                 case args of {
                 [a1] -> let a1' :: Exp inElt;
                             a1' = downcastE a1;
                             res :: Exp elt
                             res = A.fromIntegral a1'
                         in sealExp res;
                 _ -> error$ "fromIntegral operator expects one arg, got "++show args ;};};};})

-- FINSHME
               --------------------------------------------------
               _ -> error$ "Primop unhandled or got wrong argument type: "++show op++" / "++show outTy
               )
      )  -- End outTy dispatch.
      })}) -- End fstArgTy dispatch.
    -- End PrimApp case.

    S.ELet (vr,ty,rhs) bod ->
      let rhs' = cE rhs
          ep'@(EnvPack _ _ m2) = extendE vr ty rhs' ep
          resTy = scalarTypeD (S.recoverExpType (M.map P.fst m2) bod)
      in
       convertOpenExp ep' bod

    S.ECond e1 e2 e3 ->
      let d1 = cE e1
          d2 = cE e2
          d3 = cE e3
          ty = S.recoverExpType typeEnv e2
      in case scalarTypeD ty of
          SealedEltTuple (t :: EltTuple elt) ->
           -- #define a macro for this?
           case t of
             UnitTuple -> 
               sealExp(((downcastE d1::Exp Bool) A.?
                        (downcastE d2::Exp elt,
                         downcastE d3::Exp elt))::Exp elt)
             SingleTuple _ ->
               sealExp(((downcastE d1::Exp Bool) A.?
                        (downcastE d2::Exp elt,
                         downcastE d3::Exp elt))::Exp elt)
             PairTuple _ _ ->
               sealExp(((downcastE d1::Exp Bool) A.?
                        (downcastE d2::Exp elt,
                         downcastE d3::Exp elt))::Exp elt)

             ThreeTuple {} ->
               sealExp(((downcastE d1::Exp Bool) A.?
                        (downcastE d2::Exp elt,
                         downcastE d3::Exp elt))::Exp elt)


-- type OrdPrim = (forall b . Ord b => b -> b -> Exp Bool)
type OrdPrim = (forall b . (Elt b, IsScalar b) => Exp b -> Exp b -> Exp Bool)

-- | Dig down to find an Ord instance and call the given Prim:
ordPrim :: forall a . Elt a => (T.ScalarType a) -> OrdPrim -> ([SealedExp] -> SealedExp)
ordPrim inTy prim args = 
  let [se1,se2] = args in
  case inTy of 
   T.NumScalarType (T.IntegralNumType a) -> 
     case T.integralDict a of 
      T.IntegralDict -> 
        let x,y :: Exp a
            x = downcastE se1
            y = downcastE se2
        in sealExp $ prim x y

   T.NumScalarType (T.FloatingNumType a) -> 
     case T.floatingDict a of 
      T.FloatingDict -> 
        let x,y :: Exp a
            x = downcastE se1
            y = downcastE se2
        in sealExp $ prim x y

   T.NonNumScalarType a -> 
      error$"DynamicAcc2/ordPrim: attempting to apply relational operator to non-numeric type: "++show inTy

-- | The SimpleAcc representation does NOT keep index types disjoint
-- from regular tuples.  To convert back to Acc we need to reassert
-- this distinction, which involves "casting" indices to tuples and
-- tuples to indices at the appropriate places.
indexToTup :: S.Type -> SealedExp -> SealedExp
indexToTup ty ex =
  dbgtrace (" -- starting indexToTup... ")$ 
  dbgtrace (" -- indexTo tup, converting "++show (ty,ex)++" to "++ show res)
   res
  where
    res = 
     case ty of
       TTuple [] -> sealExp (constant ())

       TTuple [TInt,TInt] -> 
         let l,r :: Exp Int
             (Z :. l :. r) = unlift (downcastE ex :: Exp (Z :. Int :. Int))
             tup :: Exp (Int, Int)
             tup = lift (l, r)
         in sealExp tup

       TTuple [TInt,TInt,TInt] -> 
         let a,b,c :: Exp Int
             (Z :. a :. b :. c) = unlift (downcastE ex :: Exp (Z :. Int :. Int :. Int))
             tup :: Exp (Int, Int, Int)
             tup = lift (a,b,c)
         in sealExp tup

-- FINISHME: Go up to tuple size 9.

       TTuple ls -> error$ "indexToTup: tuple type not handled: "++show(ty,ex)
       TArray{}  -> error$ "indexToTup: expected tuple-of-scalar type, got: "++show ty
       _ ->
         case scalarTypeD ty of      
           SealedEltTuple (t :: EltTuple elt) ->
             let
                 z :: Exp (Z :. elt)
                 z = downcastE ex
                 e' :: Exp (elt)
                 Z :. e' = unlift z
             in sealExp e'

-- | The inverse of `indexToTup`.  Takes the plain SimpleAcc TTuple of
-- TInt type as the first argument.
tupToIndex :: S.Type -> SealedExp -> SealedExp
tupToIndex ty ex =
    dbgtrace (" ~~ starting tupToIndex... ")$ 
    dbgtrace (" ~~ tupToIndex tup, converting "++show (ty,ex)++" to "++ show res) res
 where
 res =  
  case ty of
    TTuple []  -> sealExp (constant Z)
    TTuple [a] -> error$ "tupToIndex: internal error, singleton tuple: "++show (ty,ex)

    TTuple [TInt,TInt] -> 
      dbgtrace ("Converting tuple type to index type... "++show ty) $
          let l,r :: Exp Int
              (l,r) = unlift (downcastE ex :: Exp (Int,Int))
              ind' :: Exp (Z :. Int :. Int)
              ind' = lift (Z :. l :. r)
          in sealExp ind'

    TTuple [TInt,TInt,TInt] -> 
      dbgtrace ("Converting tuple type to index type... "++show ty) $
          let a,b,c :: Exp Int
              (a,b,c) = unlift (downcastE ex :: Exp (Int,Int,Int))
              ind' :: Exp (Z :. Int :. Int :. Int)
              ind' = lift (Z :. a :. b :. c)
          in sealExp ind'
      
-- FINISHME: Go up to tuple size 9.

    TTuple _ -> error$"tupToIndex: unhandled tuple type: "++ show ty

    TArray{}  -> error$ "tupToIndex: expected tuple-of-scalar type, got: "++show ty
    _ -> 
      case scalarTypeD ty of      
        SealedEltTuple (t :: EltTuple elt) ->
          let
              z :: Z :. Exp elt
              z = Z :. ((downcastE ex) :: Exp elt)
              z' :: Exp (Z :. elt)
              z' = lift z
          in sealExp z'


tupTyToIndex = error "finish me"

shapeTyLen :: Type -> Int
shapeTyLen TInt        = 1
shapeTyLen (TTuple ls) | P.all (==TInt) ls = length ls
shapeTyLen ty = error $ "shapeTyLen: invalid shape type: "++show ty

convertAcc :: S.AExp -> SealedAcc
convertAcc = convertOpenAcc emptyEnvPack

-- | Convert a closed `SimpleAcc` expression (no free vars) into a fully-typed (but
-- sealed) Accelerate one.
convertOpenAcc :: EnvPack -> S.AExp -> SealedAcc
convertOpenAcc env@(EnvPack _ _ mp) ae =
  let typeEnv  = M.map P.fst mp 
      getAVr v = let (_,sa) = mp # v in expectAVar sa
  in
  case ae of
    S.Vr vr      -> getAVr vr 
    S.Use accarr -> useD accarr
    S.Unit ex ->
      let ex' = convertOpenExp env ex
          ty  = S.recoverExpType (M.map P.fst mp) ex
      in unitD (scalarTypeD ty) ex'

    S.Generate initE (S.Lam1 (vr,ty) bod) ->
      let indexTy = tupTyToIndex ty -- "ty" is a plain tuple.
          bodfn ex  = let env' = extendE vr ty (indexToTup ty ex) env in
                      dbgtrace ("Generate/bodyfun called: extended environment to: "++show env')$
                      convertOpenExp env' bod
          bodty     = S.recoverExpType (M.insert vr ty typeEnv) bod
          -- TODO: Replace this by simply passing in the result type to convertAcc:
          dims = shapeTyLen$ S.recoverExpType typeEnv initE 
          outArrTy  = TArray dims bodty
          init' = tupToIndex ty$ convertOpenExp env initE 
      in
       dbgtrace ("Generate: computed body type: "++show bodty) $ 
       generateD init' bodfn outArrTy
         
    S.Map (S.Lam1 (vr,ty) bod) inA -> 
      let bodfn ex    = convertOpenExp (extendE vr ty ex env) bod
          bodty       = S.recoverExpType (M.insert vr ty $ M.map P.fst mp) bod
      in mapD bodfn (getAVr inA) bodty

    S.ZipWith (S.Lam2 (vr1,ty1) (vr2,ty2) bod) inA inB ->  
      let bodfn e1 e2 = let env' = extendE vr2 ty2 e2 $
                                   extendE vr1 ty1 e1 env
                        in convertOpenExp env' bod
          aty1@(TArray dims1 _) = P.fst (mp # inA)
          aty2@(TArray dims2 _) = P.fst (mp # inB)
          mp' = M.insert vr2 ty2 $ 
                M.insert vr1 ty1 typeEnv
          bodty = S.recoverExpType mp' bod
      in
      assert (dims1 == dims2) $ 
      zipWithD bodfn (getAVr inA) (getAVr inB) aty1 aty2 bodty

    S.Fold (S.Lam2 (v1,ty) (v2,ty2) bod) initE inA ->
      dbgtrace ("FOLD CASE.. fold of "++show (mp # inA))$
       let init' = convertOpenExp env initE
           bodfn x y = convertOpenExp (extendE v1 ty x$ extendE v2 ty y env) bod
           aty@(TArray _ inty) = P.fst (mp # inA)
           sealedInArr = getAVr inA 
       in
        if ty /= ty2 || ty2 /= inty
        then error "Mal-formed Fold.  Input types to Lam2 must match eachother and array input."
        else foldD bodfn init' sealedInArr aty

    S.Replicate slc inE inA ->  
      replicateD (sliceTypeD slc) (convertOpenExp env inE) (getAVr inA)


    _ -> error$"FINISHME/DynamicAcc: convertOpenAcc: unhandled array operation: " ++show ae

-- | Convert an entire SimpleAcc `Prog` into a complete, closed, fully
-- typed Accelerate AST.  To use this AST, however, you will need to
-- know what type to downcast it to.
convertProg :: S.Prog () -> SealedAcc
convertProg S.Prog{progBinds,progResults} =
    dbgtrace ("CONVERTING whole prog "++show(doc progBinds)) $ 
    doBinds emptyEnvPack progBinds
  where 
  doBinds env (S.ProgBind vr ty dec eith : rst) =
    dbgtrace (" dobinds of "++show (vr,ty,rst)++" "++show env) $ 
    case eith of
      Left ex  -> let se = convertOpenExp env ex
                      env' = extendE vr ty se env
                  in doBinds env' rst
      Right ae -> let sa = convertOpenAcc env ae
                      env' = extendA vr ty sa env
                  in doBinds env' rst
  doBinds (EnvPack _ _ mp) [] =
    case S.resultNames progResults of
      [resVr] -> expectAVar$ P.snd$ mp # resVr
      _ -> error$ "FINISHME/DynamicAcc: convertProg with multiple results: "++show progResults
  


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------    

instance Show (EltTuple a) where
  show UnitTuple = "()"
  show (SingleTuple st) = show st
  show (PairTuple a b)  = "("++show a++","++show b++")"
  show (ThreeTuple a b c)  = "("++show a++","++show b++","++show c++")"

instance Show SealedEltTuple where
  show (SealedEltTuple x) = "Sealed:"++show x

instance Show SealedShapeType where
  show (SealedShapeType (Phantom :: Phantom sh)) =
    "Sealed:"++show (toDyn (unused::sh))
    
instance Show SealedArrayType where
  show (SealedArrayType (Phantom :: Phantom sh)) =
    "Sealed:"++show (toDyn (unused::sh))

--------------------------------------------------------------------------------

-- | For debugging purposes we should really never use Data.Map.!  This is an
-- alternative with a better error message.
(#) :: (Ord a1, Show a, Show a1) => M.Map a1 a -> a1 -> a
mp # k = case M.lookup k mp of
          Nothing -> error$"Map.lookup: key "++show k++" is not in map:\n  "++show mp
          Just x  -> x
