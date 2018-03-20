------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 1: 
--    Introduction
------------------------------------------------------------
module LE01Uitwerking where

import Prelude hiding (product)
import LE01

------------------------------------------------------------
-- Uitwerking

-- Opgave 1.1
oppervlakte zijde = zijde * zijde

-- Opgave 1.5
product []     = 1
product (x:xs) = x * product xs

-- Zelftoets 2
viermaal x = 4 * x