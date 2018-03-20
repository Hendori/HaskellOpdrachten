------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 2: 
--    First steps
------------------------------------------------------------
module LE02Uitwerking where

import Prelude hiding (last, init)
import LE02

------------------------------------------------------------
-- Uitwerking

-- Opgave 2.11 (Opgave 2.3 in Hutton)
n = a `div` length xs
 where
   a  = 10
   xs = [1,2,3,4,5]

-- Opgave 2.12 (Opgave 2.4 in Hutton)
last xs = head (reverse xs)

-- Zelftoets 2
middelste xs = xs !! (length xs `div` 2)

-- Zelftoets 3 (Opgave 2.5 in Hutton)
init xs = take (length xs - 1) xs