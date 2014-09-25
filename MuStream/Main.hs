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


import Data.Supply
import System.IO.Unsafe


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

ex1 :: Data (Stream Int) 
ex1 = sscan (+) 0 (smap (+1) source)

ex2 :: Data (Stream Int) 
ex2 = share source $ \a -> let b = smap (+1) a
                               c = smap (+1) a
                           in sZipWith (+) b c 




---------------------------------------------------------------------------
-- * Analysis and graph construction
---------------------------------------------------------------------------

getTree :: (Syntactic a, StringTree (Domain a)) => a -> Tree String
getTree = stringTree . desugar 




---------------------------------------------------------------------------
-- Compile 
--------------------------------------------------------------------------- 

compileAST :: (Syntactic a, Domain a ~ Lang) => a -> String
compileAST = doIt . desugar
  where doIt = undefined 
          

-- Phase1 compiled AST into a Graph representation
-- list of nodes identified by a key and a list of in-edges to that node

type GraphRep node = [(node, Integer, [Integer])]

findNode :: GraphRep node -> Integer -> Maybe (node, [Integer])
findNode [] _ = Nothing
findNode ((n,i,args):xs) key | key Prelude.== i = Just (n,args)
                             | otherwise = findNode xs key

removeNode :: GraphRep node -> Integer -> GraphRep node
removeNode [] _ = []
removeNode (a@(_,i,_):xs) key | key Prelude.== i = xs
                              | otherwise = a : removeNode xs key 


collect1 :: Eq node => GraphRep node -> node -> [Integer]
collect1 [] _ = []
collect1 ((n,i,_):xs) node | n Prelude.== node = i : collect1 xs node
                           | otherwise = collect1 xs node

repoint :: Eq node => GraphRep node -> [Integer] -> Integer -> GraphRep node 
repoint [] _ _ = [] 
repoint ((n,i,args):xs) candidate newval = (n,i,rename args):repoint xs candidate newval
  where
    rename [] = []
    rename (x:xs) = if any (Prelude.==x) candidate
                    then newval : rename xs
                    else x : rename xs 

---------------------------------------------------------------------------
-- Intermediate Code
---------------------------------------------------------------------------

-- node types                           
data Node = NSZipWith
          | NSMap
          | NSScan 
          | NLam String
          | NVar String
          | NSource
            -- Arithmetic
          | NAdd
          | NSub
          | NMul
          | NConst String 
          deriving (Eq, Ord, Show, Read) 
          
class ToNode sym where
  toNodeSym :: sym sig -> Node 

instance (ToNode sym1, ToNode sym2) => ToNode (sym1 :+: sym2) where
  toNodeSym (InjL s) = toNodeSym s
  toNodeSym (InjR s) = toNodeSym s

instance ToNode Arithmetic where
  toNodeSym Add = NAdd
  toNodeSym Sub = NSub
  toNodeSym Mul = NMul

instance ToNode StreamOp where
  toNodeSym Source = NSource
  toNodeSym SMap   = NSMap
  toNodeSym SScan  = NSScan
  toNodeSym SZipWith = NSZipWith

instance ToNode BindingT where
  toNodeSym (VarT n) =  NVar $ "v" ++ show n
  toNodeSym (LamT n) =  NLam $ "v" ++ show n 

instance ToNode Let where -- dummy 
  toNodeSym _ = error "why!?"

instance ToNode Empty where 
  toNodeSym _ = error "Empty: Why!?" 
instance ToNode Construct where
  toNodeSym (Construct n _)  = NConst  n 


---------------------------------------------------------------------------
-- Compile program to a graph 
---------------------------------------------------------------------------
class ToNode sym => Phase1 sym where
  -- update a GraphRep with a symbol 
  phase1Sym :: Supply Integer
               -> GraphRep Node
               -> [Integer]
               -> sym a
               -> (Integer, GraphRep Node)

  phase1Sym s gacc args sym = (v, new : gacc) 
    where
      new = (toNodeSym sym, v, args) 
      v =  supplyValue s 

instance (Phase1 sym1, Phase1 sym2) => Phase1 (sym1 :+: sym2) where
  phase1Sym s g args (InjL sym) = phase1Sym s g args sym
  phase1Sym s g args (InjR sym) = phase1Sym s g args sym

-- instance StringTree Let
--   where
--     stringTreeSym [a, Node lam [body]] Let
--         | ("Lam",v) <- splitAt 3 lam = Node ("Let" ++ v) [a,body]
--     stringTreeSym [a,f] Let = Node "Let" [a,f]

instance Phase1 Let where
  -- let is implemented using Lam .. (It seems)

  phase1Sym s gacc [a,body] Let =
    case findNode gacc body of
      Just (NLam variableToReplace,[realBody]) ->
       -- case splitAt 3 nodeString of
       --    ("Lam", variableToReplace) ->
            let refs = collect1 gacc (NVar variableToReplace)
                newGraph = repoint gacc refs a 
            in -- (v, ("Let", v, [a,realBody]): removeNode newGraph body)
             (realBody, removeNode newGraph body)
          -- Can this really happen ?  ( I suspect not) 
      Just (_,_) -> error "Should not happen" -- (v, ("Let", v, [a,body]): gacc)
      Nothing -> error "found Nothing"
    where
      v = supplyValue s
 -- phase1Sym s gacc args Let = (v, ("let", v, args): gacc)
 --   where
 --     v = supplyValue s

instance Phase1 BindingT
instance Phase1 Construct
instance Phase1 StreamOp
instance Phase1 Arithmetic


instance Phase1 Empty



-- Convert a AST to a Graph 
phase1 :: forall sym a . Phase1 sym => Supply Integer -> ASTF sym a -> (Integer, GraphRep Node)
phase1 s = go s emptyGraph emptyArgs 
  where
    --  Convert a symbol to a 'Graph'
    go :: Supply Integer -> GraphRep Node -> [Integer] -> AST sym sig -> (Integer,GraphRep Node)
    go s gacc args (Sym sym) =  phase1Sym s gacc args sym
    go s gacc args (sym :$ a) = go s2 (g++gacc) (t:args) sym 
      where
        (s1,s2) = split2 s 
        (t, g) = phase1 s1 a 
      
    emptyGraph = []
    emptyArgs = [] 


---------------------------------------------------------------------------
-- Utilities 
---------------------------------------------------------------------------
mySupply :: Supply Integer
mySupply = unsafePerformIO $ newEnumSupply 

---------------------------------------------------------------------------
-- MAIN MAIN MAIN  
---------------------------------------------------------------------------

main = putStrLn "Hello World"
