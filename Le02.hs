------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 2: 
--    First steps
------------------------------------------------------------
module LE02 where

-- 2.5 Haskell scripts

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
    where 
        b = 1
        c = 2
d = a * 2

