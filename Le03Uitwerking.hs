------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 3: 
--    Types and classes
------------------------------------------------------------
module LE03Uitwerking where

import LE03

------------------------------------------------------------
-- Uitwerking

-- Opgave 3.13
f :: Int -> Int
f x = x * x + 1

-- Opgave 3.14
ongelijk :: Eq a => a -> a -> a -> Bool
ongelijk x y z = (x /= y) && (x /= z) && (y /= z)

-- Opgave 3.18
machtTien :: Int -> Int
machtTien = macht 10