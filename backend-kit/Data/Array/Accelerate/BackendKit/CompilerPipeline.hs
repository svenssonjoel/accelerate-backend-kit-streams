
-- | PHASE1 :  Accelerate -> SimpleAcc
--
-- This module provides a function to convert from Accelerate's
-- internal representation to the `SimpleAcc` external representation.
-- This representation retains nearly the full set of Accelerate
-- language constructs.  Desugaring is postponed to phase 2.
module Data.Array.Accelerate.BackendKit.CompilerPipeline
       (
         -- * Major compiler phases:
         phase0, phase1, phase2, phase3,
         -- * Reexport from ToAccClone:
         unpackArray, packArray, repackAcc, Phantom,
         -- * Internal bits, exported for now:
         phase2A         
       )
       where

import           Text.PrettyPrint.GenericPretty (Out(..))
import           Text.PrettyPrint.HughesPJ (text)
import           Debug.Trace (trace)

import qualified Data.Array.Accelerate.AST         as AST
import qualified Data.Array.Accelerate.Smart       as Smt
import qualified Data.Array.Accelerate.Array.Sugar as Sug
import           Data.Array.Accelerate.Trafo.Sharing (convertAcc)
import qualified Data.Array.Accelerate.BackendKit.IRs.SimpleAcc as S
import qualified Data.Array.Accelerate.BackendKit.IRs.CLike     as C
import qualified Data.Array.Accelerate.BackendKit.IRs.GPUIR     as G
import           Data.Array.Accelerate.BackendKit.IRs.Metadata   (Stride, ArraySizeEstimate, FreeVars)
import           Data.Array.Accelerate.BackendKit.CompilerUtils  (runPass, runOptPass)

-- Phase 1 passes:
----------------------------------------
import Data.Array.Accelerate.BackendKit.Phase1.ToAccClone        (accToAccClone,unpackArray, packArray, repackAcc, Phantom)
import Data.Array.Accelerate.BackendKit.Phase1.LiftLets          (gatherLets)
import Data.Array.Accelerate.BackendKit.Phase1.LiftComplexRands  (liftComplexRands)
import Data.Array.Accelerate.BackendKit.Phase1.RemoveArrayTuple  (removeArrayTuple)
import Data.Array.Accelerate.BackendKit.Phase1.StaticTuples      (staticTuples)

-- Phase 2 passes:
----------------------------------------
import Data.Array.Accelerate.BackendKit.Phase2.DesugarUnit       (desugarUnit)
import Data.Array.Accelerate.BackendKit.Phase2.SizeAnalysis      (sizeAnalysis)
import Data.Array.Accelerate.BackendKit.Phase2.ExplicitShapes    (explicitShapes)
import Data.Array.Accelerate.BackendKit.Phase2.TrackUses         (trackUses)
import Data.Array.Accelerate.BackendKit.Phase2.FuseMaps          (fuseMaps)
import Data.Array.Accelerate.BackendKit.Phase2.DesugToBackperm   (desugToBackperm)
import Data.Array.Accelerate.BackendKit.Phase2.DesugToGenerate   (desugToGenerate)
import Data.Array.Accelerate.BackendKit.Phase2.EstimateCost      (estimateCost)
import Data.Array.Accelerate.BackendKit.Phase2.InlineCheap       (inlineCheap)
import Data.Array.Accelerate.BackendKit.Phase2.DeadCode          (deadCode)
import Data.Array.Accelerate.BackendKit.Phase2.OneDimensionalize (oneDimensionalize)
import Data.Array.Accelerate.BackendKit.Phase2.NormalizeExps     (normalizeExps)
import Data.Array.Accelerate.BackendKit.Phase2.UnzipETups        (unzipETups)
import Data.Array.Accelerate.BackendKit.Phase2.UnzipArrays       (unzipArrays)
import Data.Array.Accelerate.BackendKit.Phase2.ToCLike           (convertToCLike)

-- Phase 3 passes:
----------------------------------------
import Data.Array.Accelerate.BackendKit.Phase3.KernFreeVars      (kernFreeVars)
import Data.Array.Accelerate.BackendKit.Phase3.ToGPUIR           (convertToGPUIR)
import Data.Array.Accelerate.BackendKit.Phase3.DesugarGenerate   (desugarGenerate)
import Data.Array.Accelerate.BackendKit.Phase3.DesugarFoldScan   (desugarFoldScan)

--------------------------------------------------------------------------------
-- Exposed entrypoints for this module:
--------------------------------------------------------------------------------

-- | The final step: Lower to a GPU-targetting language.
phase3 :: C.LLProg () -> G.GPUProg (FreeVars)
phase3 prog =
  runPass    "desugarGenerate"   desugarGenerate   $     -- (freevars)
  runPass    "desugarFoldScan"   desugarFoldScan   $     -- (freevars)
  runPass    "convertToGPUIR"    convertToGPUIR    $     -- (freevars)
  runPass    "kernFreeVars"      kernFreeVars      $     -- (freevars)
  prog
  
-- | The bulk of the compilation process -- eliminate unnecessary
-- forms and lower the language.
phase2 :: S.Prog () -> C.LLProg ()
phase2 prog =
  runPass    "convertToCLike"    convertToCLike    $     -- ()
  -- todo: Verify final CLike here
  runPass    "unzipArrays"       unzipArrays       $     -- (opinputs,(subbinds,(foldstride,size)))
  runPass    "unzipETups"        unzipETups        $     -- (subbinds,(foldstride,size))
                                 typecheckPass     $     
  runPass    "normalizeExps"     normalizeExps     $     -- (foldstride,size)
  phase2A    prog

-- | Factor out this [internal] piece for use in some place(s).
phase2A :: S.Prog () -> S.Prog (Maybe (Stride S.Exp),ArraySizeEstimate)
phase2A prog =
  runPass    "typecheck2"        typecheckPass     $       
  runPass    "oneDimensionalize" oneDimensionalize $     -- (foldstride,size)
  -- todo: explicitFoldStride  
  runOptPass "deadCode"          deadCode (fmap fst) $   -- (size)
  runPass    "trackUses"         trackUses         $     -- (size,uses)
   -- NOTE INLINE CHEAP IS NOT OPTIONAL PRESENTLY! (needed for copy-prop)
--   runOptPass "inlineCheap"       inlineCheap (fmap fst) $ -- (size)
  runPass    "inlineCheap"       inlineCheap       $      -- (size)
  runPass    "estimateCost"      estimateCost      $      -- (size,cost)
  runPass    "desugtoGenerate"   desugToGenerate   $      -- (size)
  runPass    "desugToBackperm"   desugToBackperm   $      -- (size,uses)
  runOptPass "fuseMaps"          fuseMaps  id      $      -- (size,uses)
  runPass    "trackUses"         trackUses         $      -- (size,uses)
  runPass    "explicitShapes"    explicitShapes    $      -- (size)
  runPass    "sizeAnalysis"      sizeAnalysis      $      -- (size)
  runPass    "desugarUnit"       desugarUnit       $      -- ()
  prog

-- | Convert the sophisticate Accelerate-internal AST representation
--   into something very simple for external consumption.  Note: this
--   involves applying a number of lowering compiler passes.
phase1 :: (Sug.Arrays a) => AST.Acc a -> S.Prog ()
phase1 prog =
  runPass "typecheck1"           typecheckPass     $       
  runPass "removeArrayTuple"     removeArrayTuple  $ -- convert to S.Prog
  runPass "gatherLets"           gatherLets        $  
  runPass "liftComplexRands"     liftComplexRands  $  
  runPass "staticTuples"         staticTuples      $
  runPass "initialConversion"    accToAccClone     $
--  runPass "preConversion"        id                $
  prog

-- | This simply calls the Accelerate front-end with the default settings for a
-- backend-kit compiler.
phase0 :: Sug.Arrays a => Smt.Acc a -> AST.Acc a
phase0 = convertAcc
          True -- recover sharing of array computations ?
          True -- recover sharing of scalar expressions ?
          True -- always float array computations out of expressions?

  
typecheckPass :: S.Prog a -> S.Prog a
typecheckPass prog =
  case S.typecheckProg prog of
    Nothing -> prog
    Just s -> error$"Typecheck pass failed: "++s

-- instance Show a => Out (Sug.Acc a) where
--   doc       = text . show
--   docPrec _ = text . show