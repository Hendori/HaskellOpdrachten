{-# LANGUAGE NPlusKPatterns #-}
------------------------------------------------------------
-- T01211: Functioneel programmeren 
--    Open Universiteit Nederland 2012
-- 
-- Bouwsteen bij leereenheid 6: 
--    Recursive functions
------------------------------------------------------------
module Le06Uitwerking where

import Prelude hiding 
   ( product, length, reverse, (++), zip, drop, even, odd
   , (^), take, last, and, concat, replicate, (!!), elem
   )
import Le06
import Le05 (positions)

------------------------------------------------------------
-- Uitwerking

-- Opgave 1
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m^(n-1))

-- Opgave 3
laatste :: [a] -> a
laatste [x]    = x
laatste (_:xs) = laatste xs

-- Opgave 4
aantalVoorkomens :: Int -> [Int] -> Int
aantalVoorkomens _ [] = 0
aantalVoorkomens n (x:xs)
   | n == x    = 1 + aantalVoorkomens n xs
   | otherwise = aantalVoorkomens n xs

aantalVoorkomens' :: Int -> [Int] -> Int
aantalVoorkomens' n xs = length (positions n xs)

-- Opgave 5
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Opgave 6
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
 where (ys,zs) = halve xs

-- Opgave 8
take :: Int -> [a] -> [a]
take n (x:xs) = x : take (n-1) xs
take _ _      = []

last :: [a] -> a
last [x]    = x
last (_:xs) = last xs

-- Zelftoets 1
and :: [Bool] -> Bool
and []     = True
and (b:bs) = b && and bs

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem x []                 = False
elem x (y:ys) | x == y    = True
              | otherwise = elem x ys

-- Zelftoets 2
indexVan :: Eq a => a -> [a] -> Int
indexVan _ []                 = error "Niet in de lijst"
indexVan c (x:xs) | c /= x    = 1 + indexVan c xs
                  | otherwise = 0

-- Zelftoets 3
eerste :: [(a, b)] -> [a]
eerste []         = []
eerste ((u,_):xs) = u : eerste xs

tweede :: [(a, b)] -> [b]
tweede []         = []
tweede ((_,v):xs) = v : tweede xs

lijst2tupels :: [(a, b)] -> ([a], [b])
lijst2tupels xs = (eerste xs, tweede xs)

lijst2tupels' :: [(a, b)] -> ([a], [b])
lijst2tupels' xs = ([ u | (u, _) <- xs ], [ v | (_, v) <- xs ])