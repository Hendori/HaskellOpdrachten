------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 8: 
--    Declaring types and classes
------------------------------------------------------------
module Le08 where

import Prelude hiding (String, Left, Right, flip)

-- 8.1 Type declarations

type String = [Char]

type Pos = (Int, Int)

type Trans = Pos -> Pos

type Pair a = (a, a)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

-- 8.2 Data declarations

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)
move West  (x, y) = (x-1, y)

moves :: [Move] -> Pos -> Pos
moves []     p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East

data Shape = Circle Float | Rect Float Float 
   deriving (Show, Eq, Ord)

square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x*y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- 8.3 Newtype declarations

newtype NatN = N Int -- N.B. Aangepaste naam i.p.v. Nat

-- 8.4 Recursive types

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

search :: Ord a => a -> Tree a -> Bool -- N.B. gewijzigde naam (occurs voor een zoekboom)
search x (Leaf y)     = x == y
search x (Node l y r) 
   | x == y    = True
   | x < y     = search x l
   | otherwise = search x r

-- 8.5 Class and instance declarations

-- 8.6 Tautology checker: zie Le08Tautology.hs

-- 8.7 Abstract machine: zie Le08Machine.hs

-- 10.6 Class and instance declarations
--    Voor het Shape datatype zijn Show, Eq en Ord instanties afgeleid.

-- N.B. in het werkboek heet onderstaand datatype Tree
data Boom a = Blad | Knoop (Boom a) a (Boom a)
   deriving Show

instance Eq a => Eq (Boom a) where
   Blad             == Blad             = True
   Blad             == Knoop _ _ _      = False
   Knoop _ _ _      == Blad             = False
   Knoop lt1 x1 rt1 == Knoop lt2 x2 rt2 = 
      x1 == x2 && lt1 == lt2 && rt1 == rt2