
-- | The entrypoint to an Accelerate backend based on Cilk.
module Data.Array.Accelerate.Cilk (run) where

import           Data.Array.Accelerate (Acc)
import qualified Data.Array.Accelerate.Array.Sugar as Sug
import qualified Data.Array.Accelerate.Cilk.JITRuntime as J
import           Data.Array.Accelerate.Shared.EmitC (ParMode(..))


run :: Sug.Arrays a => Acc a -> a
run = J.run CilkParallel
