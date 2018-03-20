------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 4: 
--    Defining functions
------------------------------------------------------------
module LE04Uitwerking where

import Prelude hiding 
   ( splitAt, tail, cons, (||), (&&)
   )
import LE04

------------------------------------------------------------
-- Uitwerking

-- Opgave 4.1
balkOpp :: Float -> Float -> Float -> Float
balkOpp l b h = 2 * rechthoekOpp l h +
                2 * rechthoekOpp h b +
                2 * rechthoekOpp l b
                
-- Opgave 4.2
min2 :: Int -> Int -> Int
min2 x y = if x <= y then x else y

min3 :: Int -> Int -> Int -> Int
min3 x y z = min2 (min2 x y) z

min3if :: Int -> Int -> Int -> Int
min3if x y z = if x <= y
   then if x <= z then x else z
   else if y <= z then y else z
   
-- Opgave 4.3
abc :: (Float, Float, Float) -> [Float]
abc (a,b,c) = if d < 0
              then []
              else if d == 0
                   then [-b/n]
                   else [(-b-sqrtD)/n,(-b+sqrtD)/n]
 where
   d     = b^2 - 4*a*c
   sqrtD = sqrt d
   n     = 2*a
   
-- Opgave 4.4
min2' :: Int -> Int -> Int
min2' x y | x <= y    = x
          | otherwise = y

min3':: Int -> Int -> Int -> Int
min3' x y z | x <= y    = min2' x z
            | y <= z    = y
            | otherwise = z
            
-- Opgave 4.5
abc' :: Float -> Float -> Float -> [Float]
abc' a b c | d < 0     = []
           | d == 0    = [-b/n]
           | otherwise = [(-b-sqrtD)/n,(-b+sqrtD)/n]
 where
   d     = b^2 - 4*a*c
   sqrtD = sqrt d
   n     = 2*a
   
aantalWortels :: Float -> Float -> Float -> Int
aantalWortels a b c = length (abc' a b c)

aantalWortels':: Float -> Float -> Float -> Int
aantalWortels' a b c | d < 0     = 0
                     | d == 0    = 1
                     | otherwise = 2
 where
   d = b^2 - 4*a*c
   
-- Opgave 4.6
isPositief :: Int -> Bool
isPositief n | n > 0     = True
             | otherwise = False

isPositief' :: Int -> Bool
isPositief' n = n > 0

-- Opgave 4.7
testN' :: Int -> String
testN' n | n < 5     = "kleiner dan 5"
         | n < 10    = "kleiner dan 10"
         | otherwise = "groot"
         
-- Opgave 4.8
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_ ,y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

-- Opgave 4.10
dupliceerEerste :: [a] -> [a]
dupliceerEerste []     = []
dupliceerEerste (x:xs) = x:x:xs

-- Opgave 4.11
decrLinks,decrRechts :: (Int, Int) -> (Int, Int)
decrLinks (x, y)  = (x-1, y)
decrRechts (x, y) = (x, y-1)

-- Opgave 4.12
plakAchterAan :: [[a]] -> [[a]]
plakAchterAan []       = []
plakAchterAan (xs:xss) = xss++[xs]

-- Opgave 4.13
getalNaarString :: Int -> String
getalNaarString 0 = "nul"
getalNaarString 1 = "een"
getalNaarString 2 = "twee"
getalNaarString 3 = "drie"
getalNaarString _ = "Helaas, te groot"

-- Opgave 4.16
cons :: Char -> String -> String
cons x xs = x:xs

cons' :: Char -> String -> String
cons' x xs = (:) x xs

consA :: String -> String
consA = ('A':)

consEllo :: Char -> String
consEllo = (:"ello")

-- Zelftoets 1
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

halve' :: [a] -> ([a],[a])
halve' xs = (take n xs, drop n xs)
 where
   n = length xs `div` 2
   
-- Zelftoets 2
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs   = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

safetail''' :: [a] -> [a]
safetail''' []    = []
safetail'''(_:xs) = xs

-- Zelftoets 3
(||) :: Bool -> Bool -> Bool
b || c | b == c    = b
       | otherwise = True
       
-- Zelftoets 4/5
(&&) :: Bool -> Bool -> Bool -- N.B. gewijzigde naam
a && b = if a then b else False

-- Zelftoets 6
mult1 = \x -> (\y -> (\z -> x*y*z))
mult2 = \x -> \y -> \z -> x*y*z
mult3 = \x y z -> x*y*z