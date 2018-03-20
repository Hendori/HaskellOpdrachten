------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 7: 
--    Higher-order functions
------------------------------------------------------------
module Le07Uitwerking where

import Prelude hiding 
   ( map, filter, sum, product, or, and, foldr, length, reverse
   , foldl, (.), odd, id, all, any, takeWhile, dropWhile
   , curry, uncurry
   )
   
import Data.Char
import Le07
import Le07Transmit hiding ( bin2int )

------------------------------------------------------------
-- Uitwerking

-- Opgave 2
isIn :: Eq a => a -> [a] -> Bool
isIn x xs = or (map (== x) xs)

-- Opgave 3
selecteer :: Ord a => a -> a -> [a] -> [a]
selecteer m n xs = filter p xs
 where p x = x > m && x < n
 
 -- Opgave 5

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p
-- alternatief: all p xs = and (map p xs)

any :: (a -> Bool) -> [a] -> Bool
any p = or  . map p
-- alternatief: any p xs = or (map p xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []                 = []
takeWhile p (x:xs) | p x       = x : takeWhile p xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]         
dropWhile _ []                 = []
dropWhile p (x:xs) | p x       = dropWhile p xs
                   | otherwise = x : xs
                   
-- Opgave 7
map' :: (a -> b) -> [a] -> [b] -- N.B. Andere naam
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a] -- N.B. Andere naam
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- Opgave 8
minLijst :: Ord a => [a] -> a
minLijst (x:xs) = foldl min x xs

-- Opgave 10
minLijst' :: Ord a => [a] -> a
minLijst' = foldl1 min

minLijst'' :: Ord a => [a] -> a
minLijst'' = foldr1 min

-- Opgave 11
evenKwadraat :: [Int] -> [Int]
evenKwadraat = compose [map (^2), filter even]

-- Opgave 12
herhaal :: Int -> (a -> a) -> a -> a
herhaal n = compose . replicate n

herhaal' :: Int -> (a -> a) -> a -> a
herhaal' n f | n > 0     = f . herhaal' (n-1) f
             | otherwise = id
             
-- Opgave 13
bin2int :: [Bit] -> Int
bin2int bits = sum [ w*b | (w, b) <- zip weights bits ]
 where weights = iterate (*2) 1
 
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

-- Opgave 15
char2bin :: Char -> [Bit]
char2bin = int2bin . ord

bin2char :: [Bit] -> Char
bin2char = chr . bin2int

-- Opgave 17
chop8U :: [a] -> [[a]] -- N.B. andere naam
chop8U = unfold null (take 8) (drop 8)

mapU :: (a -> b) -> [a] -> [b] -- N.B. andere naam
mapU f = unfold null (f . head) tail

iterateU :: (a -> a) -> a -> [a] -- N.B. andere naam
iterateU f = unfold (const False) id f

-- Zelftoets 2
dec2nat :: [Int] -> Int
dec2nat = foldl (\x y -> 10*x + y) 0

machten10 :: Num a => [a] -> [a]
machten10 []     = []
machten10 (x:xs) = x : machten10 (map (*10) xs)

dec2nat' :: [Int] -> Int
dec2nat' = sum . machten10 . reverse

-- Zelftoets 3
combineer :: (b -> c) -> (a -> b) -> [a] -> [c]
combineer f g xs = map f (map g xs)

combineer' :: (b -> c) -> (a -> b) -> [a] -> [c]
combineer' f g = map (f . g)

-- Zelftoets 4
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x,y) -> f x y

-- Zelftoets 5
encodeP :: String -> [Bit]
encodeP = concat . map (addparity . make8 . int2bin . ord)

decodeP :: [Bit] -> String
decodeP = map (chr . bin2int . checkparity) . chop9

addparity :: [Bit] -> [Bit]
addparity bs = (parity bs) : bs

parity :: [Bit] -> Bit
parity bs | odd (sum bs) = 1
          | otherwise    = 0
          
chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkparity :: [Bit] -> [Bit]
checkparity (b:bs)
   | b == parity bs = bs
   | otherwise      = error "parity mismatch"

transmitP :: String -> String
transmitP = decodeP . channel . encodeP

channelF :: [Bit] -> [Bit]
channelF = tail

transmitF :: String -> String
transmitF = decodeP . channelF . encodeP
