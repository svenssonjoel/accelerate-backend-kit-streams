{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE DeriveDataTypeable #-}


module Main where



import Prelude hiding (max, min, not, (==), length, map, sum, zip, zipWith)
import qualified Prelude

import Data.Tree
import Data.Typeable

import Data.Syntactic hiding (fold, printExpr, showAST, drawAST, writeHtmlAST)
import qualified Data.Syntactic as Syntactic
import Data.Syntactic.Functional
import Data.Syntactic.Sugar.BindingT



--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

-- | Convenient class alias
class    (Typeable a, Show a, Eq a, Ord a) => Type a
instance (Typeable a, Show a, Eq a, Ord a) => Type a

type Length = Int
type Index  = Int



--------------------------------------------------------------------------------
-- * Arithmetic
--------------------------------------------------------------------------------

data Arithmetic a where
  Add :: (Type a, Num a) => Arithmetic (a :-> a :-> Full a)
  Sub :: (Type a, Num a) => Arithmetic (a :-> a :-> Full a)
  Mul :: (Type a, Num a) => Arithmetic (a :-> a :-> Full a)

instance Render Arithmetic where
  renderSym Add = "(+)"
  renderSym Sub = "(-)"
  renderSym Mul = "(*)"
  renderArgs = renderArgsSmart

interpretationInstances ''Arithmetic

instance Eval Arithmetic where
  evalSym Add = (+)
  evalSym Sub = (-)
  evalSym Mul = (*)

instance EvalEnv Arithmetic env where
  compileSym p Add = compileSymDefault p Add
  compileSym p Sub = compileSymDefault p Sub
  compileSym p Mul = compileSymDefault p Mul 

--------------------------------------------------------------------------------
-- * Allow let bindings
--------------------------------------------------------------------------------
data Let a
  where
    Let :: Let (a :-> (a -> b) :-> Full b)

instance Equality Let
  where
    equal = equalDefault
    hash  = hashDefault

instance Render Let
  where
    renderSym Let = "letBind"

instance StringTree Let
  where
    stringTreeSym [a, Node lam [body]] Let
        | ("Lam",v) <- splitAt 3 lam = Node ("Let" ++ v) [a,body]
    stringTreeSym [a,f] Let = Node "Let" [a,f]

instance Eval Let
  where
    evalSym Let = flip ($)

instance EvalEnv Let env
  where
    compileSym p Let = compileSymDefault p Let

--------------------------------------------------------------------------------
-- * Streams 
--------------------------------------------------------------------------------

--something to represent streams while evaluating
data Stream a = Stream [a]
        deriving (Show, Ord, Eq,  Typeable)

-- type Stream a = [a] 
unstream (Stream xs) = xs
stream xs = Stream xs 


data StreamOp a where
  Source :: StreamOp (Full (Stream Int)) 
  SMap   :: StreamOp ((a -> b) :-> Stream a :-> Full (Stream b))
  SScan  :: StreamOp ((a -> b -> a) :-> a :-> Stream b :-> Full (Stream a))
  SZipWith :: StreamOp ((a -> b -> c) :-> Stream a :-> Stream b :-> Full (Stream c)) 
  
instance Render StreamOp where
  renderSym Source = "Source"
  renderSym SMap   = "SMap"
  renderSym SScan  = "SScan"
  renderSym SZipWith = "SZipWith" 
  renderArgs = renderArgsSmart

interpretationInstances ''StreamOp

instance Eval StreamOp where
  evalSym Source = stream [1..]
  evalSym SMap   = \f s -> stream $ Prelude.map f (unstream s)
  evalSym SScan  = \f init s -> stream $ Prelude.scanl f init (unstream s)
  evalSym SZipWith = \f s1 s2 -> stream $ Prelude.zipWith f (unstream s1) (unstream s2) 

instance EvalEnv StreamOp env where
  compileSym p Source = compileSymDefault p Source
  compileSym p SMap   = compileSymDefault p SMap
  compileSym p SScan  = compileSymDefault p SScan
  compileSym p SZipWith = compileSymDefault p SZipWith


--------------------------------------------------------------------------------
-- * Domain
--------------------------------------------------------------------------------

type Lang = Arithmetic :+: StreamOp :+: Construct :+: BindingT :+: Let


newtype Data a = Data { unData :: ASTF Lang a}

-- Data is syntactic sugar
instance Type a => Syntactic (Data a) where
  type Domain (Data a) = Lang
  type Internal (Data a) = a
  desugar = unData
  sugar = Data


class    (Syntactic a, Domain a ~ Lang, Type (Internal a)) => Syntax a
instance (Syntactic a, Domain a ~ Lang, Type (Internal a)) => Syntax a


instance Type a => Show (Data a)
  where
    show = render . unData

--------------------------------------------------------------------------------
-- * "Backends"
--------------------------------------------------------------------------------

-- | Show the expression
showExpr :: (Syntactic a, Domain a ~ Lang) => a -> String
showExpr = render . desugar

-- | Print the expression
printExpr :: (Syntactic a, Domain a ~ Lang) => a -> IO ()
printExpr = putStrLn . showExpr

-- | Show the syntax tree using unicode art
showAST :: (Syntactic a, Domain a ~ Lang) => a -> String
showAST = Syntactic.showAST . desugar

-- | Draw the syntax tree on the terminal using unicode art
drawAST :: (Syntactic a, Domain a ~ Lang) => a -> IO ()
drawAST = putStrLn . showAST

-- | Write the syntax tree to an HTML file with foldable nodes
writeHtmlAST :: (Syntactic a, Domain a ~ Lang) => a -> IO ()
writeHtmlAST = Syntactic.writeHtmlAST "tree.html" . desugar

eval :: (Syntactic a, Domain a ~ Lang) => a -> Internal a
eval = evalClosed . desugar


--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------


source :: Data (Stream Int) 
source = sugarSym Source

smap :: (Type a, Type b) =>  (Data a -> Data b) -> Data (Stream a) -> Data (Stream b) 
smap = sugarSym SMap 

sscan :: (Type a, Type b) => (Data a -> Data b -> Data a) -> Data a -> Data (Stream b) -> Data (Stream a)
sscan = sugarSym SScan 

sZipWith :: (Type a, Type b, Type c) => (Data a -> Data b -> Data c) -> Data (Stream a) -> Data (Stream b) -> Data (Stream c) 
sZipWith = sugarSym SZipWith 

value :: Syntax a => Internal a -> a
value a = sugar $ inj $ Construct (show a) a

instance (Type a, Num a) => Num (Data a)
  where
    fromInteger = value . fromInteger
    (+)         = sugarSym Add
    (-)         = sugarSym Sub
    (*)         = sugarSym Mul

share :: (Syntax a, Syntactic b, Domain b ~ Lang) => a -> (a -> b) -> b
share = sugarSym Let



--------------------------------------------------------------------------------
-- * Example 
--------------------------------------------------------------------------------

ex1 = sscan (+) 0 (smap (+1) source)


ex2 = share source $ \a -> let b = smap (+1) a
                               c = smap (+1) a
                           in sZipWith (+) b c 





---------------------------------------------------------------------------
-- * Analysis and graph construction
---------------------------------------------------------------------------



