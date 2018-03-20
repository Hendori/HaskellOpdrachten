------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 15: 
--    Lazy evaluation
------------------------------------------------------------
module Le15Uitwerking where

import Le15

------------------------------------------------------------
-- Uitwerking

-- Opgave 4
twice :: Int -> Int
twice x = x + x

-- Opgave 5
keerPositie :: [Int] -> [Int]
keerPositie ys = [ x*y | (x, y) <- zip [1..] ys ]

keerPositie' :: [Int] -> [Int]
keerPositie' = zipWith (*) [1..]


-- Opgave 6
machten :: Int -> [Int]
machten n = iterate (*n) 1

machten' :: Int -> [Int]
machten' n = [n^p | p <- [0..]]

eersteMachten :: Int -> Int -> [Int]
eersteMachten i = take i . machten

somEersteMachten :: Int -> Int -> Int
somEersteMachten i = sum . eersteMachten i

-- Opgave 10
volgende :: Double -> Double -> Double
volgende n x = (x + n/x) / 2

-- Opgave 11
herhaal :: (Double -> Double) -> Double -> [Double]
herhaal f x = x : herhaal f (f x)

-- Opgave 12
goedGenoeg :: Double -> [Double] -> Double
goedGenoeg eps (x1:x2:xs)
   | abs (x1-x2) <= eps = x2
   | otherwise          = goedGenoeg eps (x2:xs)

wortel :: Double -> Double -> Double -> Double
wortel x0 eps n = goedGenoeg eps (herhaal (volgende n) x0)

-- Zelftoets 5
fibs :: [Integer]
fibs = 0 : 1 : [ x+y | (x, y) <- zip fibs (tail fibs) ]

-- Zelftoets 6
fib :: Int -> Integer
fib n = fibs !! n

eersteGroterDanDuizend :: Integer
eersteGroterDanDuizend = head (dropWhile (<=1000) fibs)

-- Zelftoets 7
data Tree a = Leaf | Node (Tree a) a (Tree a)
   deriving Show

repeatTree :: a -> Tree a
repeatTree x = Node t x t
   where t = repeatTree x
   
takeTree :: Int -> Tree a -> Tree a
takeTree 0     _            = Leaf
takeTree (n+1) Leaf         = Leaf
takeTree (n+1) (Node l x r) = Node (takeTree n l) x (takeTree n r)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree
