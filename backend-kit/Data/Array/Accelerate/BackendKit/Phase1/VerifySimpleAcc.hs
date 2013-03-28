{-# LANGUAGE NamedFieldPuns #-}

-- | This module provides a pass that performs type checking and other
-- invariant-validation for the SimpleAcc datatype.
--
-- It does NOT encode all the (shifting) invariants between different passes of the
-- compiler, just the ones that always hold.
module Data.Array.Accelerate.BackendKit.Phase1.VerifySimpleAcc
       (
         verifySimpleAcc, VerifierConfig(..)
       )
       where

import Data.Map as M
import Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as S
import Data.Array.Accelerate.BackendKit.SimpleArray (verifyAccArray)
import Prelude as P hiding (or) 
--------------------------------------------------------------------------------

-- | It would be difficult to capture all the changing grammars/invarinats between
-- passes, but we do parameterize this pass by some choices:
data VerifierConfig =
  VerifierConfig {
    -- ^ If False, all Array dimensions should be 1 
    multiDim :: Bool
  }

type Env = M.Map Var Type
type ErrorMessage = String

-- Attempt to typecheck a program, returning Nothing if it checks out,
-- or an error message if there is a probem.
verifySimpleAcc :: VerifierConfig -> Prog a -> Maybe String
verifySimpleAcc cfg prog@Prog{progBinds, progResults, progType } =
  -- The rule for progResults is that their types match a flattened version of the result type:
    (assertTyEq "Result type" resTys expectedTys) `or`
    (not (all hasArrayType resTys) ? 
      ("Final result of program includes non-array:"
       ++show(P.filter (not . hasArrayType) resTys))) `or`
    -- (verifyUnique "Result names" (resultNames progResults)) `or`
    -- (verifyUnique "Top level bindings" topBinds) `or`
    (doBinds cfg env progBinds)
  where
    resTys      = P.map envlkp (resultNames progResults)
    envlkp vr   = case M.lookup vr env of
                    Nothing -> error$"SimpleAcc.hs/typeCheckProg: no binding for progResult: "++show vr
                    Just x  -> x 
    expectedTys = flattenTy progType
    env         = progToEnv prog

    topBinds = []


verifyUnique = error "verifyUnique"

mismatchErr :: (Show a, Show a1) => String -> a -> a1 -> String
mismatchErr msg got expected = msg++" does not match expected. "++
                               "\nGot:      "++show got ++
                               "\nExpected: "++show expected

assertTyEq :: (Eq a, Show a) => String -> a -> a -> Maybe String
assertTyEq msg got expected =
  if got == expected
  then Nothing
  else Just$ mismatchErr msg got expected

doBinds :: VerifierConfig -> Env -> [ProgBind t] -> Maybe ErrorMessage
doBinds _cfg _env [] = Nothing
doBinds cfg env (ProgBind vo ty _ (Right ae) :rst) =
  doAE ty env ae `or`
  doBinds cfg env rst
doBinds cfg env (ProgBind vo ty _ (Left ex) :rst) =
  assertTyEq ("Top-level scalar variable "++show vo)
             (recoverExpType env ex) ty `or`
  doBinds cfg env rst

-- TODO: Simplify handling of AExps by introducing an arrow type, and coming up with
-- some direct representation of the type of each AExp op.
-- data TmpType = Arrow TmpType TmpType | TVar Int | PlainType Type

doAE :: Type -> Env -> AExp -> Maybe ErrorMessage
doAE outTy env ae =
  case ae of
    Use arr -> verifyAccArray outTy arr
    Vr v    -> lkup v $ \ty -> assertTyEq ("Varref "++show v) ty outTy
    Map fn vr               ->      
      let (it,ot,err) = typeFn1 "Map" fn in
      err `or`
      assertTyEq "Map result" ot elty `or`
      arrVariable vr (TArray ndim it)

    ZipWith fn2 v1 v2   ->
      let (it1,it2,ot,err) = typeFn2 "ZipWith" fn2 in
      err `or`
      assertTyEq "ZipWith result" ot elty `or`
      arrVariable v1 (TArray ndim it1) `or`
      arrVariable v2 (TArray ndim it2)
      
    Cond e1 v1 v2 -> expr "Array-level conditional test" e1 TBool `or`
                     arrVariable v1 outTy `or`
                     arrVariable v2 outTy 
    Generate e1 fn1 ->
      let (it,ot,err) = typeFn1 "Map" fn1
          e1ty = recoverExpType env e1 in 
      err `or`
      assertTyEq "Generate index exp" e1ty it  `or`
      assertTyEq "Generate output" (TArray ndim ot) outTy 

    Fold fn e1 vr ->
      let (elt,err) = foldHelper "Fold" fn vr in
      err `or` expr  "Fold initializer" e1 elt
    Fold1  fn vr ->
      snd$ foldHelper "Fold1" fn vr
      
--     FoldSeg fn e1 v1 v2 -> addArrRef v1 $ addArrRef v2 $
--                            doE e1       $ doFn2 fn mp 
--     Fold1Seg  fn v1 v2  -> addArrRef v1 $ addArrRef v2 $ doFn2 fn mp
--     Scanl  fn2 e1  vr   -> addArrRef vr $ doE e1       $ doFn2 fn2 mp
--     Scanl' fn2 e1  vr   -> addArrRef vr $ doE e1       $ doFn2 fn2 mp
--     Scanl1 fn2     vr   -> addArrRef vr $                doFn2 fn2 mp
--     Scanr  fn2 e1  vr   -> addArrRef vr $ doE e1       $ doFn2 fn2 mp
--     Scanr' fn2 e1  vr   -> addArrRef vr $ doE e1       $ doFn2 fn2 mp
--     Scanr1 fn2     vr   -> addArrRef vr $                doFn2 fn2 mp
--     Permute fn2 v1 fn1 v2 -> addArrRef v1 $ addArrRef v2 $
--                              doFn1 fn1    $ doFn2 fn2 mp
--     Backpermute e1 fn1 vr -> addArrRef vr $ doE e1 $ doFn1 fn1 mp
--     Stencil fn1 _b vr     -> addArrRef vr $ doFn1 fn1 mp
--     Stencil2 fn2 _b1 v1 _b2 v2 -> addArrRef v1 $ addArrRef v2 $ doFn2 fn2 mp

--     Reshape e1 vr       -> addArrRef vr $ doE e1 mp
--     Replicate _slt e1 vr -> addArrRef vr $ doE e1 mp
--     Index _slt vr e1     -> addArrRef vr $ doE e1 mp
--     Unit _ -> error "trackUses: Unit is not part of the grammar accepted by this pass"
    _  ->  Nothing
    
 where
   foldHelper variant fn vr =    
      let (it1,it2,ot,err) = typeFn2 variant fn in
      (it1,
       err `or`
       assertTyEq (variant++" arguments not the same type") it1 it2 `or`
       assertTyEq (variant++" output not expected type") it1 ot `or`
       assertTyEq (variant++" output") (TArray ndim ot) outTy
-- TODO: Check the dimension differently based on multiDim config parameter:
{-       
       `or` arrVariable vr (TArray (ndim+1) it1) -- Knock off one dim.
-}      
      )
   TArray ndim elty = outTy

   arrVariable vr (TArray expected_dim expected_elt) =
     (lkup vr $ \ argty -> 
        case argty of
          TArray ndim' elty' ->
            assertTyEq "Array variable element type" elty' expected_elt `or`
            assertTyEq "Array variable dimension" ndim' expected_dim)

   expr msg ex expected =
     assertTyEq msg (recoverExpType env ex) expected
   
   lkup v k =
     case M.lookup v env of
       Just x  -> k x
       Nothing -> Just$ "Unbound variable: "++show v
      
   typeFn1 :: String -> Fun1 Exp -> (Type,Type,Maybe ErrorMessage)
   typeFn1 msg (Lam1 (vr,inTy) exp) =
     let env' = M.insert vr inTy env
         ty'  = recoverExpType env' exp 
         err1 = case M.lookup vr env of
                Just _  -> Just (msg++": Local formal param is not globally unique: "++show vr) 
                Nothing -> Nothing
     in (inTy, ty',
         err1 `or` doE env' exp)

   -- TODO: factor these into one variant that takes a list of formals:
   typeFn2 :: String -> Fun2 Exp -> (Type,Type,Type,Maybe ErrorMessage)
   typeFn2 msg (Lam2 (vr1,inTy1) (vr2,inTy2) bod) =
     let env' = M.insert vr1 inTy1 $
                M.insert vr2 inTy2 env
         ty'  = recoverExpType env' bod
         err1 = case M.lookup vr1 env of
                Just _  -> Just (msg++": Local formal param is not globally unique: "++show vr1) 
                Nothing -> Nothing
         err2 = case M.lookup vr2 env of
                Just _  -> Just (msg++": Local formal param is not globally unique: "++show vr2) 
                Nothing -> Nothing                
     in (inTy1, inTy2, ty',
         err1 `or` err2 `or` doE env' bod)

-- doFn1  mp = doE exp mp 

-- doFn2 :: Fun2 Exp -> UseMap -> UseMap
-- doFn2 (Lam2 _ _ exp) mp = doE exp mp 


-- TODO -- need to replace "recoverExpType" with something that will NOT throw a
-- Haskell error.
doE _ _ = Nothing

-------------------------------------------------------------------------------

or :: Maybe ErrorMessage -> Maybe ErrorMessage -> Maybe ErrorMessage 
or (Just x) (Just y) = Just (x++"\n"++y)
or (Just x) Nothing = Just x
or Nothing (Just x) = Just x
or Nothing Nothing  = Nothing

(?) :: Bool -> ErrorMessage -> Maybe ErrorMessage
True ? m  = Just m
False ? _ = Nothing
