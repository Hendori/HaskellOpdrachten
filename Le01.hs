------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 1: 
--    Introduction
------------------------------------------------------------
module LE01 where

import Prelude hiding (sum)

-- 1.1 Functions

double x = x + x

-- 1.2 Functional programming

nummers n = [1..n]

-- 1.5 A taste of Haskell

sum []     = 0
sum (n:ns) = n + sum ns

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
 where
   smaller = [ a | a <- xs, a <= x ]
   larger  = [ b | b <- xs, b > x  ]
   
-- Zelftoets

lengte []     = 0
lengte (x:xs) = 1 + lengte xs
