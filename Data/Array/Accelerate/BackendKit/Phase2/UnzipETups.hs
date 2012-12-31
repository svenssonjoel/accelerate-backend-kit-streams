{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | This file contains a pass for removing scalar tuples.
-- 
-- It also assumes a number of invariants on the input grammar: that
-- several forms have been desugared by previous passes, and that all
-- ELet's are lifted so that they will never be passed as an argument
-- to a primitive.

-- The problem here is that functions parameterizing a Generate or
-- Fold may return scalar tuples.

module Data.Array.Accelerate.BackendKit.Phase2.UnzipETups (unzipETups) where
import Control.Monad.State.Strict
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map              as M

import Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as S
import Data.Array.Accelerate.BackendKit.Utils.Helpers (GensymM, genUnique, mkPrj, mapMAEWithGEnv, isTupleTy)
import Data.Array.Accelerate.BackendKit.Phase2.NormalizeExps (wrapLets)
----------------------------------------------------------------------------------------------------

-- | Map the original possibly-tuple-valued variable names to the
--   individual subcomponent names.  If the Var is bound to a tuple,
--   but it CANNOT yet be detupled (see caveats below), then we'll see
--   a Nothing here.
type Env = M.Map Var (Type,Maybe [Var])

----------------------------------------------------------------------------------------------------

-- | This pass is similar to the `removeArrayTuple` pass, but at the
--   scalar level.  It removes ETuple and ETupProject forms, but not
--   ALL of them.  It leaves stylized uses:
--
--    (1) A single ETuple is permitted in the tail position of each kernel.
--    (2) A single ETupProject is permitted around EIndexScalar expressions.
--    (3) ETuple's are also allowed in the tail of "spine" conditionals 
--        (i.e. in RHS of Let).  In this case the ELet IS still allowed to 
--        bind a tuple variable, and specific (ETupProject _ 1 (EVr _)) forms
--        will refer to its components.
--        (Top-level scalar bindings are likewise allowed to remain tuples.)
--    (4) ETuple may occur directly as an argument to EIndexScalar.
--    (5) Unit values, ETuple [], persist.
--
--  ALL ETupProject's after this pass must have length=1.
--
--  The reason for the first two bulets that we are not yet
--  /committing/ to having an unzipped array representation in the
--  backend.  The language of this pass's output represents the last
--  point at which we could employ an array-of-structs backend.  The
--  scalar tuples are gone *within* kernels, but the association
--  between the components of an array-of-structs is still there.
unzipETups :: Prog a -> Prog a
unzipETups prog@Prog{progBinds, uniqueCounter} = 
  prog{ progBinds= binds, 
        uniqueCounter= newCounter2 }
 where
   -- NOTE -- We can't detuple ALL top level scalar binds, so right now we don't do ANY.
  -- TODO, this should use traverseWithKey as of containers-0.5:
  envM = M.fromList <$> mapM fn (M.toList (progToEnv prog))

  -- Here we introduce temporary names for the subcomponents of any top-level tuple-bindings:
  -- fn (vr,ty) | countVals ty == 1 = return (vr,(ty,Just [vr]))
  --            | otherwise = do tmps <- sequence$ replicate (countVals ty) genUnique
  --                             return (vr,(ty,Just tmps))

  -- NEW POLICY: Do not untuple ANY of the top level bindings yet.
  -- Kick it down the road to ConvertLLIR.
  fn (vr,ty) = return (vr,(ty,Nothing))  
  
  (env, newCounter1)  = runState envM uniqueCounter
  (binds,newCounter2) = runState (doBinds env progBinds) newCounter1
   

doBinds :: Env -> [ProgBind t] -> GensymM [ProgBind t]
doBinds env = mapM (doBind env)

doBind :: Env -> ProgBind a -> GensymM (ProgBind a)
doBind env (ProgBind v t dec (Left ex))  = ProgBind v t dec . Left  <$>
   doSpine env ex

doBind env (ProgBind v t dec (Right ae)) = ProgBind v t dec . Right <$>
   -- The following MUST be *Nothing* because we have no way to detuple
   -- the input to kernels (i.e. array elements) at this point.
   mapMAEWithGEnv env (\ _ ty -> (ty,Nothing))
                  doSpine ae

   
-- | Process along the spine (which will become Stmts in the LLIR).
doSpine :: Env -> Exp -> GensymM Exp
doSpine env ex =
  case ex of
    EVr vr                -> return$ mkETuple $ handleVarref env vr
    EConst _c             -> return ex
    -- In all three of the following we allow tuples to remain:
    ETuple els            -> (mkETuple . concat) <$> mapM (doE env) els
    ETupProject i l e     -> mkETuple <$> doProject env i l e 
    EIndexScalar avr indE -> (EIndexScalar avr . mkETuple) <$> doE env indE
    -- In tail (or "spine") position multiple values may be returned by the branches:
    ECond e1 e2 e3        -> ECond <$> (unsing <$> doE env e1) <*> doSpine env e2 <*> doSpine env e3

    -- EConds in the RHS of a let are still on the "spine":
    ELet (v,t,ECond a b c) bod -> do
      [a'] <- doE env a
      b'   <- doSpine env b
      c'   <- doSpine env c
      let env' = M.insert v (t,Nothing) env
      ELet (v,t,ECond a' b' c') <$> doSpine env' bod
    
    ELet (v,t,rhs) bod | not (isTupleTy t) -> defaultRet
                       | otherwise -> -- Here's where we split the variable if we can:
                         case rhs of
                           ECond _ _ _ -> defaultRet
                           _ -> do let tyLs = flattenTypes t
                                   gensyms <- sequence$ replicate (length tyLs) genUnique
                                   rhsLs <- doE env rhs
                                   let env3 = M.insert v (t,Just gensyms) env
                                   case fragileZip3 gensyms tyLs rhsLs of
                                     Just ls -> wrapLets ls <$> doSpine env3 bod
                                     Nothing -> error$"UnzipETups.hs: expected tuple-producing expression to break down "
                                                ++show(length gensyms)++" expressions, instead got: "++show rhsLs
     where env2 = M.insert v (t,Nothing) env
           defaultRet = do [rhs'] <- doE env rhs
                           ELet (v,t,rhs') <$> doSpine env2 bod
    -- No PrimApp's expect tuple arguments:                                
    EPrimApp ty p els ->  EPrimApp ty p <$> mapM (fmap unsing . doE env) els
    EShape     _ -> err ex
    EShapeSize _ -> err ex
    EIndex     _ -> err ex

-- | Expand a varref to a tuple to a tuple of varrefs to the components.
blowUpVarref :: Var -> Type -> [Exp]
blowUpVarref vr ty = 
  let size = length $ flattenTypes ty 
  in reverse [ mkPrj ind 1 size (EVr vr) | ind <- [ 0 .. size-1 ]]

-- | A variable reference either uses the old name or uses one of the
-- fresh gensyms that refer to the components of a tuple.
handleVarref :: Env -> Var -> [Exp]
handleVarref env vr =
  case M.lookup vr env of
    Just (ty,Nothing)   -> blowUpVarref vr ty
    Just (_,Just subVs) -> map EVr subVs
    Nothing -> error$"UnzipETups.hs: internal error, var not in env: "++show vr 
  
-- | When processing non-spine expressions no ETuple's may survive.
-- This function returns a detupled (flattened) list of expressions.
doE :: Env -> Exp -> GensymM [Exp]
doE env ex = 
  case ex of
    EConst c             -> return $ map EConst (flattenConst c)
    ETuple []            -> return []
    ETuple els           -> concat <$> mapM (doE env) els
    -- Remember: ETupProject operates on the flattened list-of-scalars structure, which is what we have here:
    ETupProject i l e    -> doProject env i l e
    EVr vr               -> return$ handleVarref env vr
    --------------------------------------------------------------------------------
    -- As long as arrays remain multidimensional, indices can remain tuples:
    EIndexScalar avr indE -> (sing . EIndexScalar avr . mkETuple) <$> doE env indE 

    -- Because of the normalization phase, we know this conditional
    -- has no Lets in its branches, AND it does *not* have a tuple return type:
    ECond e1 e2 e3        -> do [e1'] <- doE env e1
                                [e2'] <- doE env e2
                                [e3'] <- doE env e3
                                return [ECond e1' e2' e3']
    -- None of the primitives operate on tuples (in or out):
    EPrimApp ty p els     -> (sing . EPrimApp ty p) <$> mapM (fmap unsing . doE env) els
    EShape     _ -> err ex
    EShapeSize _ -> err ex
    EIndex     _ -> err ex
    ELet    _  _ -> error$"UnzipETups.hs: ELet should not occur off the programs spine: "++ show ex

-- | This does the projection statically if possible, but (due to
-- blowUpVarref above), it may actually return an ETupProject.
doProject :: Env -> Int -> Int -> Exp -> GensymM [Exp]
doProject env i l e = 
  do ls <- doE env e
     let haul = reverse$ take l$ drop i$ reverse ls
     return$ if length haul == l
             then haul
             else error$"UnzipETups.hs/doProject: not enough elements in tuple: "
                  ++show ls++", needed offset/len "++show (i,l)++", expression "++show e

--------------------------------------------------------------------------------
-- Little helpers:
--------------------------------------------------------------------------------

flattenTypes :: Type -> [Type]
flattenTypes (TTuple ls) = concatMap flattenTypes ls
flattenTypes oth         = [oth]

flattenConst :: Const -> [Const]
flattenConst (Tup ls) = concatMap flattenConst ls
flattenConst c        = [c]

countVals :: Type -> Int
countVals (TTuple ls) = sum$ map countVals ls
countVals _           = 1 
  
err :: Show a => a -> t
err ex = error$"UnzipETups.hs: this form should have been desugared before this pass: "++show ex

unsing :: Show a => [a] -> a
unsing [x] = x
unsing ls  = error$"UnzipETups.hs: expected singleton list, got: "++show ls

sing :: a -> [a]
sing x = [x]


-- This one mandates that all three lists be the same 
fragileZip3 :: [t] -> [t1] -> [t2] -> Maybe [(t, t1, t2)]
fragileZip3 a b c = loop a b c
  where
    loop [] [] []                = Just []
    loop (h1:t1) (h2:t2) (h3:t3) = ((h1,h2,h3) :) <$> loop t1 t2 t3
    loop _ _ _                   = Nothing