------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 16: 
--    Reasoning about programs
------------------------------------------------------------
module Le16Uitwerking where

import Prelude hiding (replicate, reverse, last, sum)
import Le16 hiding (macht)

------------------------------------------------------------
-- Uitwerking

-- Opgave 1
-- Overlappende patronen
last :: [a] -> a
last [x] = x
last (x:xs) = last xs

-- Met verwijderde overlap
last' :: [a] -> a
last' [x]        = x
last' (x1:x2:xs) = last' (x2:xs)

-- Alternatief
last'' :: [a] -> a
last'' (x:xs) | null xs = x
              | otherwise = last'' xs

-- Opgave 2
macht :: Int -> Int -> Int
macht x 0         = 1
macht x n | n > 0 = x * macht x (n-1)

-- Opgave 5
sub :: Nat -> Nat -> Nat
sub Zero     _        = Zero
sub (Succ n) Zero     = Succ n
sub (Succ n) (Succ m) = sub n m

-- Opgave 10
sum' :: Int -> [Int] -> Int
sum' n []    = n
sum' n (x:xs) = sum' (n+x) xs

sum :: [Int] -> Int
sum xs = sum' 0 xs

-- Opgave 11
showTree' :: Tree -> String -> String
showTree' (Leaf n)   s = show n ++ s
showTree' (Node l r) s = '(' : showTree' l (',' : showTree' r (')' : s))

showTree :: Tree -> String
showTree t = showTree' t ""

-- Zelftoets 5
leaves :: Tree -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree -> Int
nodes (Leaf _)   = 0
nodes (Node l r) = 1 + nodes l + nodes r