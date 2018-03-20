------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 15: 
--    Lazy evaluation
------------------------------------------------------------
module Le15 where

import Prelude hiding (take, replicate)

-- 15.1 Introduction
inc :: Int -> Int
inc n = n + 1

-- 15.2 Evaluation strategies

fst :: (a, b) -> a
fst (x, y) = x

kwadraat :: Int -> Int
kwadraat x = x*x

mult :: Int -> Int -> Int
mult x = \y -> x*y

mult' :: Int -> Int -> Int
mult' = \x -> \y -> x*y

-- Opgave 15.2
add :: Int -> Int -> Int
add x y = x + y

-- 15.3 Termination

inf :: Int
inf = 1 + inf

-- 15.4 Number of reductions

square :: Int -> Int
square n = n * n

-- 15.5 Infinite structures

ones :: [Int]
ones = 1 : ones

-- 15.6 Modular programming

take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

-- 15.7 Strict application

sumwith :: Int -> [Int] -> Int
sumwith v []     = v
sumwith v (x:xs) = (sumwith $! (v+x)) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = ((foldl' f) $! (f v x)) xs

sumwith2 :: Int -> [Int] -> Int
sumwith2 = foldl' (+)