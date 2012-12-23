{-# LANGUAGE OverloadedStrings #-}

module Data.Array.Accelerate.Shared.EmitHelpers
       (
         -- * Some help with code-emission:
         emitPrimApp, emitCType, emitOpenCLType,
         builderName,

         -- * Miscellaneous helpers
         fragileZip
       )
       where


import Data.Array.Accelerate.Shared.EasyEmit as EE

import qualified Data.Map as M
import Data.Array.Accelerate.BackendKit.IRs.SimpleAcc  as S
import Text.PrettyPrint.HughesPJ as PP
import Foreign.Storable (sizeOf)
import qualified Prelude as P
import Prelude (error, ($), (.))
import Data.Int (Int)
import Data.Word (Word)
import Prelude ((++), show, return, Show)
import Control.Monad.State.Strict (State, get, put)
import Control.Applicative ((<$>),(<*>),pure,Applicative)


----------------------------------------------------------------------------------------------------
-- Emission:
----------------------------------------------------------------------------------------------------


-- | During final C/OpenCL emission, create a name for a function that
-- executes a specific array operator.  That is, if you know the name
-- of an array variable, this will tell you what function to call to
-- populate that array.
builderName :: Var -> P.String
builderName vr = "build_" P.++ P.show vr


-- | Emit a PrimApp provided that the operands have already been convinced to `Syntax`.
--   It returns EasyEmit `Syntax` representing a C expression.
--
--   The contract of this function is that the code generated by it
--   should be CAST to the expected type.
emitPrimApp :: Prim -> [Syntax] -> Syntax
emitPrimApp p args =
  case p of
    NP np -> case np of
              Add -> binop "+"
              Sub -> binop "-"
              Mul -> binop "*"
              Neg -> unary "-"
              Abs -> unary "abs"
              -- Warning, potential for code duplication here.  Should ensure that args are trivial:
              Sig ->  arg && (arg>0) && (-(arg<0))
    IP ip -> case ip of
              -- This uses the stdlib.h div function, not available in OpenCL:
              -- Quot -> (binfun "div") `dot` (constant "quot")
              -- Rem  -> (binfun "div") `dot` (constant "rem")
              Quot -> binop "/"
              Rem  -> binop "%"
              -- These two need to round towards negative infinity:
              IDiv -> error "integer division truncated towards negative infinity... not implemented yet!"
              Mod  -> error "integer modulus truncated towards negative infinity... not implemented yet!"
              BAnd -> binop "&"
              BOr  -> binop "|"
              BXor -> binop "^"
              BNot -> unary  "~"
              BShiftL -> binop "<<"
              BShiftR -> binop ">>"
              BRotateL -> (left << right) .| (left >> ((sizeof right) * 8 - 1))
              BRotateR -> (left >> right) .| (left << ((sizeof right) * 8 - 1))
    FP p -> case p of
              Recip -> EE.parens (1 / arg) 
              Sin  -> unary "sin"
              Cos  -> unary "cos"
              Tan  -> unary "tan"
              Asin -> unary "asin"
              Acos -> unary "acos"
              Atan -> unary "atan"
              Asinh -> unary "asinh"
              Acosh -> unary "acosh"
              Atanh -> unary "atanh"
              ExpFloating -> binop ""
              Sqrt  -> binop "sqrt"
              Log   -> binop "log" -- natural log
              FDiv    -> binop "/"
              FPow    -> binfun "expt"
              LogBase -> binop "log"
              Atan2   -> unary "atan2"
              Round   -> unary "round"
              Floor   -> unary "floor"
              Ceiling -> unary "ceil"
              -- The C CAST that should be wrapped around the esult of
              -- emitPrimApp should effectively truncate:
              Truncate -> arg
    SP p -> case p of
              Lt   -> binop "<"
              Gt   -> binop ">"
              LtEq -> binop "<="
              GtEq -> binop ">="
              Eq   -> binop "=="
              NEq  -> binop "!="
              Max  -> binfun "max"
              Min  -> binfun "min"
    BP p -> case p of
              And  -> binop "&&"
              Or   -> binop "||"
              Not  -> unary "!" 
    OP p -> case p of
              FromIntegral -> arg -- Again, depend on the cast.
              BoolToInt    -> arg
              Ord          -> arg
              S.Chr        -> arg
  where
   t = text
   [left,right] = args
   [arg]        = args -- laziness in action

   argD   = fromSyntax arg
   leftD  = fromSyntax left
   rightD = fromSyntax right
   
   -- No parens for a binop, that is handled by the caller of `emitPrimApp`:
   binop op  = left +++ toSyntax (text (" "++op++" ")) +++ right
   binfun op = toSyntax (text op <> PP.parens (leftD <> comma <> rightD))
   unary  op = toSyntax$ text op <> PP.parens argD


-- | Convert a type to the equivalent C type.
emitCType :: Type -> Syntax
-- NOTE! In the future this will have to grow more complex to track dimension:
emitCType (TArray dim elt) = emitCType elt +++ str "*"
emitCType ty = toSyntax$ text$ 
  case ty of
    TInt   -> "int"
    TInt8  -> "int8_t"
    TInt16 -> "int16_t"
    TInt32 -> "int32_t"
    TInt64 -> "int64_t"
    TWord   -> "unsigned int"
    TWord8  -> "uint8_t"
    TWord16 -> "uint16_t"
    TWord32 -> "uint32_t"
    TWord64 -> "uint64_t"
    TCShort  -> "short"
    TCInt  -> "int"
    TCLong  -> "long"
    TCLLong -> "long long"
    TCUShort -> "unsigned short"
    TCUInt -> "unsigned int"
    TCULong -> "unsigned long"
    TCULLong -> "unsigned long long"
    TCChar    -> "char"
    TCUChar   -> "unsigned char"
    TCSChar   -> "char"
    TFloat     -> "float"
    TCFloat    -> "float"
    TDouble     -> "double"
    TCDouble    -> "double"
    TChar       -> "char"
    TBool       -> "bool"
    TTuple [] -> "void"
    TTuple _  -> error "emitType: cannot handle tuples presently"

-- | Convert a type to the equivalent OpenCL type.  Note that unlike
-- plain C, OpenCL provides specific guarantees as to the size of
-- standard numeric types like "int".  Thus this function differs
-- significantly from its counterpart for plain C types.
emitOpenCLType :: Type -> Syntax
-- NOTE! In the future this will have to grow more complex to track dimension:
emitOpenCLType (TArray dim elt) = emitOpenCLType elt +++ "*"
emitOpenCLType ty = toSyntax$ text$ 
  case ty of
    -- This is the size of a HASKELL Int:
    TInt   -> case sizeOf(0::Int) of
                4 -> "int"
                8 -> "long" -- In GHC, unlike C, Ints are 64 bit on a 64 bit platform.
                oth -> error$"unexpected Int byte size: " P.++ P.show oth
    TWord   -> case sizeOf(0::Word) of
                4 -> "unsigned int"
                8 -> "unsigned long"
                oth -> error$ "unexpected Word byte size: " P.++ P.show oth
    TInt8  -> "char"
    TInt16 -> "short"
    TInt32 -> "int"
    TInt64 -> "long"    
    TWord8  -> "unsigned char"
    TWord16 -> "unsigned short"
    TWord32 -> "unsigned int"
    TWord64 -> "unsigned long"
    TCShort -> "short"
    TCInt   -> "int"
    TCLong  -> "int"
    TCLLong -> "long"
    TCUShort -> "unsigned short"
    TCUInt   -> "unsigned int"
    TCULong  -> "unsigned int"
    TCULLong -> "unsigned long"
    TCChar   -> "char"
    TCUChar  -> "unsigned char"
    TCSChar  -> "char"
    TFloat   -> "float"
    TCFloat  -> "float"
    TDouble  -> "double"
    TCDouble -> "double"
    TChar    -> "char"
    TBool    -> "bool"
    TTuple [] -> "void"
    TTuple _  -> error "emitOpenCLType: cannot handle tuples presently"
    TArray dim elt -> error "cannot happen"


str = toSyntax . text

test0 = emitPrimApp (NP Sig) [constant "x"]
test1 = emitPrimApp (IP Quot) [constant "x", constant "y"]


-- | Do not allow the lists to be different lengths.
fragileZip :: (Show t1, Show t2) =>
              [t1] -> [t2] -> [(t1, t2)]
fragileZip a b = loop a b
  where
    loop [] []           = []
    loop (h1:t1) (h2:t2) = (h1,h2) : loop t1 t2
    loop _ _             = error$"JIT.hs/fragileZip: lists were not the same length: "++show a++" "++show b

