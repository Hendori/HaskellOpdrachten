------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 16: 
--    Reasoning about programs
------------------------------------------------------------
module Le16 where

import Prelude hiding (replicate, reverse, last, sum, fmap, elem)

-- 16.2 Reasoning about Haskell

double :: Int -> Int
double x = x + x

isZero 0          = True
isZero n | n /= 0 = False

-- 16.3 Simple examples

-- Opgave 2
macht :: Int -> Int -> Int
macht x 0 = 1
macht x n = x * macht x (n-1)

-- 16.4 Induction on numbers

data Nat = Zero | Succ Nat

add Zero     m = m
add (Succ n) m = Succ (add n m)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

fmap :: (a -> b) -> [a] -> [b]
fmap g []     = []
fmap g (x:xs) = g x : fmap g xs

-- 16.5 Induction on lists
-- Opgave 8
elem :: Eq a => a -> [a] -> Bool
elem n [] = False
elem n (x:xs) = n == x || elem n xs

-- 16.6 Making append vanish

reverse' :: [a] -> [a] -> [a]
reverse' [] ys     = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

reverse :: [a] -> [a]
reverse xs = reverse' xs [] 

data Tree = Leaf Int | Node Tree Tree

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n)   ns = n:ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flatten :: Tree -> [Int]
flatten t = flatten' t []

-- Opgave 10
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

-- Opgave 11
showTree :: Tree -> String
showTree (Leaf n) = show n
showTree (Node l r) = "(" ++ showTree l ++ "," ++ showTree r ++ ")"

-- 16.7 Compiler correctness: zie Le16Compiler.hs