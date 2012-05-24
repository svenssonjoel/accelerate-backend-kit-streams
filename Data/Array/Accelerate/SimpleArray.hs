{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Utilities for working with the simplified representation of
--   Accelerate arrays on the Haskell heap.
module Data.Array.Accelerate.SimpleArray
   ( 
          
     -- * Runtime Array data representation.
     AccArray(..), ArrayPayload(..),  -- Reexported from SimpleAST
          
     -- * Functions for operating on `AccArray`s
     mapArray,
     splitComponent, numComponents,

     -- * Functions for constructing `AccArray`s
     Data.Array.Accelerate.SimpleArray.replicate,      
     
     -- * Functions for operating on payloads (internal components of AccArrays)     
     payloadToList, payloadFromList,
     payloadLength, 
     mapPayload, 
     
     applyToPayload, applyToPayload2, applyToPayload3,                                           
   )
   
where 
  

import Data.Array.Accelerate.SimpleAST 
  
import           Debug.Trace
import           Data.Int
import           Data.Word
import           Data.Array.Unboxed as U
import qualified Data.Array.Unsafe as Un
import qualified Data.Array.MArray as MA
import qualified Data.Array.IO     as IA
import           Foreign.C.Types 
import           Pretty (text) -- ghc api
import           Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map          as M
  

----------------------------------------------------------------------------------------------------
-- Helper functions for working with Array data living on the Haskell heap:
----------------------------------------------------------------------------------------------------

-- | How many elements are in the payload?  This handles the annoying
--   large case dispatch on element type.
payloadLength :: ArrayPayload -> Int
payloadLength payl =
  case payl of 
--    ArrayPayloadUnit       -> 0
    ArrayPayloadInt    arr -> arrLen arr
    ArrayPayloadInt8   arr -> arrLen arr
    ArrayPayloadInt16  arr -> arrLen arr
    ArrayPayloadInt32  arr -> arrLen arr
    ArrayPayloadInt64  arr -> arrLen arr
    ArrayPayloadWord   arr -> arrLen arr
    ArrayPayloadWord8  arr -> arrLen arr
    ArrayPayloadWord16 arr -> arrLen arr
    ArrayPayloadWord32 arr -> arrLen arr
    ArrayPayloadWord64 arr -> arrLen arr
    ArrayPayloadFloat  arr -> arrLen arr
    ArrayPayloadDouble arr -> arrLen arr
    ArrayPayloadChar   arr -> arrLen arr
    ArrayPayloadBool   arr -> arrLen arr
 
-- | Apply a generic transformation to the Array Payload irrespective
--   of element type.  This is useful for rearranging or removing
--   elements of a payload without looking at the contents.
applyToPayload :: (forall a . UArray Int a -> UArray Int a) -> ArrayPayload -> ArrayPayload
applyToPayload fn payl = applyToPayload2 (\ a _ -> fn a) payl


-- | This is similar to `applyToPayload`, but also provides the ability for
--   the function passed in to inspect elements in the input array in a
--   generic fashion (as Const type).
applyToPayload2 :: (forall a . UArray Int a -> (Int -> Const) -> UArray Int a) -> ArrayPayload -> ArrayPayload
applyToPayload2 fn payl = 
  case payl of 
--    ArrayPayloadUnit       -> ArrayPayloadUnit
    ArrayPayloadInt    arr -> ArrayPayloadInt    (fn arr (\i -> I (arr U.! i)))
    ArrayPayloadInt8   arr -> ArrayPayloadInt8   (fn arr (\i -> I8 (arr U.! i)))
    ArrayPayloadInt16  arr -> ArrayPayloadInt16  (fn arr (\i -> I16 (arr U.! i)))
    ArrayPayloadInt32  arr -> ArrayPayloadInt32  (fn arr (\i -> I32 (arr U.! i))) 
    ArrayPayloadInt64  arr -> ArrayPayloadInt64  (fn arr (\i -> I64 (arr U.! i)))
    ArrayPayloadWord   arr -> ArrayPayloadWord   (fn arr (\i -> W (arr U.! i)))
    ArrayPayloadWord8  arr -> ArrayPayloadWord8  (fn arr (\i -> W8 (arr U.! i))) 
    ArrayPayloadWord16 arr -> ArrayPayloadWord16 (fn arr (\i -> W16 (arr U.! i)))
    ArrayPayloadWord32 arr -> ArrayPayloadWord32 (fn arr (\i -> W32 (arr U.! i)))
    ArrayPayloadWord64 arr -> ArrayPayloadWord64 (fn arr (\i -> W64 (arr U.! i)))
    ArrayPayloadFloat  arr -> ArrayPayloadFloat  (fn arr (\i -> F (arr U.! i)))
    ArrayPayloadDouble arr -> ArrayPayloadDouble (fn arr (\i -> D (arr U.! i)))
    ArrayPayloadChar   arr -> ArrayPayloadChar   (fn arr (\i -> C (arr U.! i)))
    ArrayPayloadBool   arr -> ArrayPayloadBool -- Word8's represent bools
                              (fn arr (\i -> case arr U.! i of
                                               0 -> B False 
                                               _ -> B True))

-- | This version allows the payload to be rebuilt as a list of Const,
--   which must all be the same type as the input.
applyToPayload3 :: (Int -> (Int -> Const) -> [Const]) -> ArrayPayload -> ArrayPayload
-- TODO!! The same-type-as-input restriction could be relaxed.
applyToPayload3 fn payl = 
  case payl of 
--    ArrayPayloadUnit       -> ArrayPayloadUnit
    ArrayPayloadInt    arr -> ArrayPayloadInt    (fromL (map unI  $ fn len (\i -> I   (arr U.! i))))
    ArrayPayloadInt8   arr -> ArrayPayloadInt8   (fromL (map unI8 $ fn len (\i -> I8  (arr U.! i))))
    ArrayPayloadInt16  arr -> ArrayPayloadInt16  (fromL (map unI16$ fn len (\i -> I16 (arr U.! i))))
    ArrayPayloadInt32  arr -> ArrayPayloadInt32  (fromL (map unI32$ fn len (\i -> I32 (arr U.! i))))
    ArrayPayloadInt64  arr -> ArrayPayloadInt64  (fromL (map unI64$ fn len (\i -> I64 (arr U.! i))))
    ArrayPayloadWord   arr -> ArrayPayloadWord   (fromL (map unW  $ fn len (\i -> W   (arr U.! i))))
    ArrayPayloadWord8  arr -> ArrayPayloadWord8  (fromL (map unW8 $ fn len (\i -> W8  (arr U.! i))))
    ArrayPayloadWord16 arr -> ArrayPayloadWord16 (fromL (map unW16$ fn len (\i -> W16 (arr U.! i))))
    ArrayPayloadWord32 arr -> ArrayPayloadWord32 (fromL (map unW32$ fn len (\i -> W32 (arr U.! i))))
    ArrayPayloadWord64 arr -> ArrayPayloadWord64 (fromL (map unW64$ fn len (\i -> W64 (arr U.! i))))
    ArrayPayloadFloat  arr -> ArrayPayloadFloat  (fromL (map unF  $ fn len (\i -> F   (arr U.! i))))
    ArrayPayloadDouble arr -> ArrayPayloadDouble (fromL (map unD  $ fn len (\i -> D   (arr U.! i))))
    ArrayPayloadChar   arr -> ArrayPayloadChar   (fromL (map unC  $ fn len (\i -> C   (arr U.! i))))
    ArrayPayloadBool   arr -> ArrayPayloadBool   (fromL (map fromBool$ fn len (\i -> toBool (arr U.! i))))
  where 
   len = payloadLength payl


-- Various helpers:
----------------------------------------
fromL l = U.listArray (0,length l - 1) l
toBool 0 = B False
toBool _ = B True
fromBool (B False) = 0
fromBool (B True)  = 1
unI   (I x) = x
unI8  (I8 x) = x
unI16 (I16 x) = x
unI32 (I32 x) = x
unI64 (I64 x) = x
unW   (W x) = x
unW8  (W8 x) = x
unW16 (W16 x) = x
unW32 (W32 x) = x
unW64 (W64 x) = x
unF (F x) = x
unD (D x) = x
unC (C x) = x
unB (B x) = x
-- Length of a UArray:
arrLen arr = let (st,en) = U.bounds arr in en - st      


-- | Apply an elementwise function to each Const inside an array.  The
--   mapped function must consistently map the same type of input
--   Const to the same type of output Const, or a runtime error will
--   be generated.
mapArray :: (Const -> Const) -> AccArray -> AccArray
mapArray fn (AccArray sh pls) = 
  AccArray sh (map (mapPayload fn) pls)

-- | Apply an elementwise function to a single payload.  The function
-- must consistently map the same type of input to the same type of
-- output `Const`.
mapPayload :: (Const -> Const) -> ArrayPayload -> ArrayPayload
mapPayload fn payl =   
--  tracePrint ("\nMapPayload of "++show payl++" was : ") $ 
  case payl of        
    ArrayPayloadInt   arr -> rebuild (fn . I  $ arr U.! 0) (fn . I  ) arr
    ArrayPayloadInt8  arr -> rebuild (fn . I8 $ arr U.! 0) (fn . I8 ) arr
    ArrayPayloadInt16 arr -> rebuild (fn . I16$ arr U.! 0) (fn . I16) arr
    ArrayPayloadInt32 arr -> rebuild (fn . I32$ arr U.! 0) (fn . I32) arr
    ArrayPayloadInt64 arr -> rebuild (fn . I64$ arr U.! 0) (fn . I64) arr
    ArrayPayloadWord   arr -> rebuild (fn . W  $ arr U.! 0) (fn . W  ) arr
    ArrayPayloadWord8  arr -> rebuild (fn . W8 $ arr U.! 0) (fn . W8 ) arr
    ArrayPayloadWord16 arr -> rebuild (fn . W16$ arr U.! 0) (fn . W16) arr
    ArrayPayloadWord32 arr -> rebuild (fn . W32$ arr U.! 0) (fn . W32) arr
    ArrayPayloadWord64 arr -> rebuild (fn . W64$ arr U.! 0) (fn . W64) arr
    ArrayPayloadFloat  arr -> rebuild (fn . F  $ arr U.! 0) (fn . F) arr
    ArrayPayloadDouble arr -> rebuild (fn . D  $ arr U.! 0) (fn . D) arr
    ArrayPayloadChar   arr -> rebuild (fn . C  $ arr U.! 0) (fn . C) arr
    ArrayPayloadBool   arr -> rebuild (fn . W8 $ arr U.! 0) (fn . W8) arr
    
{-# INLINE rebuild #-}
rebuild :: IArray UArray e' => Const -> (e' -> Const) -> UArray Int e' -> ArrayPayload
rebuild first fn arr = 
  case first of
    I   _ -> ArrayPayloadInt    $ amap (unI   . fn) arr
    I8  _ -> ArrayPayloadInt8   $ amap (unI8  . fn) arr    
    I16 _ -> ArrayPayloadInt16  $ amap (unI16 . fn) arr    
    I32 _ -> ArrayPayloadInt32  $ amap (unI32 . fn) arr    
    I64 _ -> ArrayPayloadInt64  $ amap (unI64 . fn) arr    
    W   _ -> ArrayPayloadWord   $ amap (unW   . fn) arr
    W8  _ -> ArrayPayloadWord8  $ amap (unW8  . fn) arr    
    W16 _ -> ArrayPayloadWord16 $ amap (unW16 . fn) arr    
    W32 _ -> ArrayPayloadWord32 $ amap (unW32 . fn) arr    
    W64 _ -> ArrayPayloadWord64 $ amap (unW64 . fn) arr        
    F   _ -> ArrayPayloadFloat  $ amap (unF   . fn) arr
    D   _ -> ArrayPayloadDouble $ amap (unD   . fn) arr
    C   _ -> ArrayPayloadChar   $ amap (unC   . fn) arr
    B   _ -> ArrayPayloadBool   $ amap (unW8  . fn) arr
    c     -> error$"This constant should not be found inside an ArrayPayload: "++show c


-- | Convert a list of `Const` scalars of the same type into a
--   `ArrayPayload`.  Keep in mind that this is an inefficient thing
--   to do, and in performant code you should never convert arrays to
--   or from lists.
payloadFromList :: [Const] -> ArrayPayload
payloadFromList [] = error "payloadFromList: cannot convert empty list -- what are the type of its contents?"
payloadFromList ls@(hd:_) = 
  case hd of 
    I   _ -> ArrayPayloadInt    $ fromL $ map unI   ls
    I8  _ -> ArrayPayloadInt8   $ fromL $ map unI8  ls
    I16 _ -> ArrayPayloadInt16  $ fromL $ map unI16 ls
    I32 _ -> ArrayPayloadInt32  $ fromL $ map unI32 ls
    I64 _ -> ArrayPayloadInt64  $ fromL $ map unI64 ls
    W   _ -> ArrayPayloadWord   $ fromL $ map unW   ls
    W8  _ -> ArrayPayloadWord8  $ fromL $ map unW8  ls
    W16 _ -> ArrayPayloadWord16 $ fromL $ map unW16 ls
    W32 _ -> ArrayPayloadWord32 $ fromL $ map unW32 ls
    W64 _ -> ArrayPayloadWord64 $ fromL $ map unW64 ls
    F   _ -> ArrayPayloadFloat  $ fromL $ map unF   ls
    D   _ -> ArrayPayloadDouble $ fromL $ map unD   ls
    C   _ -> ArrayPayloadChar   $ fromL $ map unC   ls
    B   _ -> ArrayPayloadBool   $ fromL $ map fromBool ls
    c     -> error$"This constant should not be found inside an ArrayPayload: "++show c
    
  
-- | Unpack a payload into a list of Const.  Inefficient!
payloadToList :: ArrayPayload -> [Const]
payloadToList payl =   
  case payl of        
    ArrayPayloadInt    arr -> map I  $ elems arr
    ArrayPayloadInt8   arr -> map I8 $ elems arr
    ArrayPayloadInt16  arr -> map I16$ elems arr
    ArrayPayloadInt32  arr -> map I32$ elems arr
    ArrayPayloadInt64  arr -> map I64$ elems arr
    ArrayPayloadWord   arr -> map W  $ elems arr
    ArrayPayloadWord8  arr -> map W8 $ elems arr
    ArrayPayloadWord16 arr -> map W16$ elems arr
    ArrayPayloadWord32 arr -> map W32$ elems arr
    ArrayPayloadWord64 arr -> map W64$ elems arr
    ArrayPayloadFloat  arr -> map F  $ elems arr
    ArrayPayloadDouble arr -> map D  $ elems arr
    ArrayPayloadChar   arr -> map C  $ elems arr
    ArrayPayloadBool   arr -> map toBool $ elems arr


-- | Create an array of with the given dimensions and many copies of
--   the same element.  This deals with constructing the appropriate
--   type of payload to match the type of constant (which is otherwise
--   a large case statement).
replicate :: [Int] -> Const -> AccArray 
replicate dims const = AccArray dims (payload const)
  where 
    len = foldl (*) 1 dims
    payload const = 
     case const of 
       I   x -> [ArrayPayloadInt   (fromL$ Prelude.replicate len x)]
       I8  x -> [ArrayPayloadInt8  (fromL$ Prelude.replicate len x)]
       I16 x -> [ArrayPayloadInt16 (fromL$ Prelude.replicate len x)]
       I32 x -> [ArrayPayloadInt32 (fromL$ Prelude.replicate len x)]
       I64 x -> [ArrayPayloadInt64 (fromL$ Prelude.replicate len x)]
       W   x -> [ArrayPayloadWord   (fromL$ Prelude.replicate len x)]
       W8  x -> [ArrayPayloadWord8  (fromL$ Prelude.replicate len x)]       
       W16 x -> [ArrayPayloadWord16 (fromL$ Prelude.replicate len x)]
       W32 x -> [ArrayPayloadWord32 (fromL$ Prelude.replicate len x)]
       W64 x -> [ArrayPayloadWord64 (fromL$ Prelude.replicate len x)]
       F x -> [ArrayPayloadFloat  (fromL$ Prelude.replicate len x)]
       D x -> [ArrayPayloadDouble (fromL$ Prelude.replicate len x)]
       C x -> [ArrayPayloadChar   (fromL$ Prelude.replicate len x)]
       B x -> [ArrayPayloadBool   (fromL$ Prelude.replicate len (fromBool const))]
       Tup ls -> concatMap payload ls 

-- TODO -- add all C array types to the ArrayPayload type:
--            | CF CFloat   | CD CDouble 
--            | CS  CShort  | CI  CInt  | CL  CLong  | CLL  CLLong
--            | CUS CUShort | CUI CUInt | CUL CULong | CULL CULLong
--            | CC  CChar   | CSC CSChar | CUC CUChar 

-- | An AccArray stores an array of tuples.  This function reports how
--   many components there are in the stored tuples (one or more).
numComponents :: AccArray -> Int
numComponents (AccArray _ payloads) = length payloads


-- | Split one component (the first) out of an AccArray which
--   represents an array of tuples.  This returns two new AccArrays,
--   the first of which is a scalar, and the second of which contains
--   all remaining components.
-- 
--   If there are less than two components, this function raises a
--   runtime error.
splitComponent :: AccArray -> (AccArray, AccArray)
splitComponent (AccArray sh (h1:h2:rst)) = 
  (AccArray sh [h1], AccArray sh (h2:rst))
splitComponent x@(AccArray _ ls) = 
  error$ "splitComponent: input array has only "++show(length ls)++
         " components, needs at least two:\n   "++ show x
