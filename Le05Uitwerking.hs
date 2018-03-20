------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 5: 
--    List comprehensions
------------------------------------------------------------
module Le05Uitwerking where

import Prelude hiding (concat, length, replicate)
import Data.Char
import Le05 hiding (factors, positions, find, count)
import Le05Cipher hiding (freqs)

------------------------------------------------------------
-- Uitwerking

-- Opgave 5.1
allemaalEven :: [Int] -> Bool
allemaalEven xs = and [ even x | x <- xs ]

-- Opgave 5.2
codes :: [(Int, Int, Int)]
codes = [ (x, y, z) | x <- [0, 1], y <- [0, 1], z <- [0, 1] ]

bin2dec :: [(Int, Int, Int)] -> [Int]
bin2dec cs = [ x*4 + y*2 + z | (x, y, z) <- cs ]

-- Opgave 5.3
zoek :: [Int] -> [Int]
zoek xs = [ x | x <- xs, x > 5, x < 10 ]

-- Opgave 5.4
voorkomens :: Eq a => a -> [a] -> [a]
voorkomens x ys = [ y | y <- ys, y == x ]

isIn :: Eq a => a -> [a] -> Bool
isIn x ys = voorkomens x ys /= []

isIn' :: Eq a => a -> [a] -> Bool
isIn' x ys = not (null (voorkomens x ys))

-- Opgave 5.7
evenElementen :: [a] -> [a]
evenElementen xs = [x | (x, i) <- zip xs [1..n], even i]
 where n = length xs

-- Opgave 5.8
nummers :: [Int]
nummers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

prijzen :: [Float]
prijzen = [0.10, 0.10, 0.10, 0.10, 0.10, 2.00, 0.10, 2.00, 0.10, 2.00]

selecteer :: [Int] -> [Float] -> [(Int, Float)]
selecteer artikelnrs prijzen = [ (n, p) | (n, p) <- zip artikelnrs prijzen, even n, p < 1.0 ]

-- Opgave 5.9
gewichten :: Int -> [Int]
gewichten n = reverse [ 10^x | x <- [0..n-1] ]

stringToInt :: String -> Int
stringToInt xs = sum [ g * digitToInt x | (g, x) <- zip gs xs ]
 where gs = gewichten (length xs)
 
-- Zelftoets 1
somEerste100Kwadraten :: Int
somEerste100Kwadraten = sum [ x^2 | x <- [1..100] ]

-- Zelftoets 2
replicate :: Int -> a -> [a]
replicate n x = [ x | _ <- [1..n] ]

-- Zelftoets 3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n]
                      , y <- [1..n]
                      , z <- [1..n]
                      , x^2 + y^2 == z^2
          ]
          
-- Zelftoets 4
perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (init (factors x)) == x ]

factors :: Int -> [Int] -- N.B. gewijzigde naam, zie factors in Le05
factors n = [ x | x <- [1..n], mod n x == 0 ]

-- Zelftoets 5
zs :: [(Int, Int)]
zs = concat [ [(x, y) | y <- [3, 4] ] | x <- [1, 2] ]

-- Zelftoets 6
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

find :: Eq a => a -> [(a, b)] -> [b]
find k ts = [ v | (k',v) <- ts, k == k' ]

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..n])
 where n = length xs - 1
 
-- Zelftoets 7
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x*y | (x, y) <- zip xs ys ] 

-- Zelftoets 8
low2int :: Char -> Int
low2int c = ord c - ord 'a'

int2low :: Int -> Char
int2low n = chr (ord 'a' + n)

upp2int :: Char -> Int
upp2int c = ord c - ord 'A'

int2upp :: Int -> Char
int2upp n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2low ((low2int c + n) `mod` 26)
          | isUpper c = int2upp ((upp2int c + n) `mod` 26)
          | otherwise = c
          
letters :: String -> Int
letters xs = length [ x | x <- xs, isAlpha x ]

freqs :: String -> [Float]
freqs xs = [ percent (count x xs') n | x <- ['a'..'z'] ]
 where
   xs' = [ toLower x | x <- xs ]
   n   = letters xs
   
freqsLetter :: String -> [Float]
freqsLetter xs = freqs [ toLower x | x <- xs ]