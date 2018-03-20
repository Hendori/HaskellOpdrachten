------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 8: 
--    Declaring types and classes
-- 
-- Originele code: 
-- Abstract machine example from chapter 8 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
--    http://www.cs.nott.ac.uk/~gmh/book.html
------------------------------------------------------------
module Le08Machine where

data Expr = Val Int | Add Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value :: Expr -> Int
value e = eval e []
