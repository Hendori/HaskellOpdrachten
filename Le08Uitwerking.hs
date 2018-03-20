------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 8: 
--    Declaring types and classes
------------------------------------------------------------
module Le08Uitwerking where

import Prelude hiding (String, Left, Right, flip)
import Le08 hiding (List, Nil, Cons, Assoc, find)
import Le08Tautology

------------------------------------------------------------
-- Uitwerking

-- Opgave 2
vindAlle :: Eq k => k -> Assoc k v -> [v]
vindAlle sleutel lijst = [ w | (s, w) <- lijst, s == sleutel ]

-- Opgave 5
data ShapeP a = CircleP a | RectP a a -- N.B. gewijzigde naam

areaP :: Floating a => ShapeP a -> a
areaP (CircleP r) = pi * r^2
areaP (RectP x y) = x*y

-- Opgave 6
testGetal :: Int -> Int -> Maybe Int
testGetal n x | x > 0 && x < n = Just x
              | otherwise      = Nothing
              
-- Opgave 10
list2datalist :: [a] -> List a
list2datalist []     = Nil
list2datalist (x:xs) = Cons x (list2datalist xs)

datalist2list :: List a -> [a]
datalist2list Nil         = []
datalist2list (Cons x xs) = x : datalist2list xs

-- Opgave 13
searchC :: Ord a => a -> Tree a -> Bool -- N.B. gewijzigde naam (occurs voor een zoekboom)
searchC x (Leaf y)     = x == y
searchC x (Node l y r) = case compare x y of
                            LT -> searchC x l
                            EQ -> True
                            GT -> searchC x r
                            
-- Opgave 14
data List a = Nil | Cons a (List a)
  deriving Show

instance Eq a => Eq (List a) where
   Nil       == Nil       = True
   Nil       == _         = False
   _         == Nil       = False
   Cons x xs == Cons y ys = x == y && xs == ys

-- Opgave 15
instance Eq Prop where
  p == q = isTaut (And (Imply p q) (Imply q p))

-- Opgave 18
data Prop' = Const' Bool
           | Var' Char
           | Not' Prop'
           | And' Prop' Prop'
           | Imply' Prop' Prop'
           | Or Prop' Prop'
           | Equiv Prop' Prop'

eval' :: Subst -> Prop' -> Bool
eval' _ (Const' b)   = b
eval' s (Var' x)     = find x s
eval' s (Not' p)     = not (eval' s p)
eval' s (And' p q)   = eval' s p && eval' s q
eval' s (Imply' p q) = eval' s p <= eval' s q
eval' s (Or p q)    = eval' s p || eval' s q
eval' s (Equiv p q) = eval' s p == eval' s q

vars' :: Prop' -> [Char]
vars' (Const' _)   = []
vars' (Var' x)     = [x]
vars' (Not' p)     = vars' p
vars' (And' p q)   = vars' p ++ vars' q
vars' (Imply' p q) = vars' p ++ vars' q
vars' (Or p q)    = vars' p ++ vars' q
vars' (Equiv p q) = vars' p ++ vars' q

-- Zelftoets 1
mult :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m (Succ n) = add m (mult m n)

getal2 = Succ (Succ Zero)
getal3 = Succ (Succ (Succ Zero))

-- Zelftoets 2
data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a) -- N.B. gewijzigde naam

leaves :: Tree3 a -> Int
leaves (Leaf3 _)   = 1
leaves (Node3 l r) = leaves l + leaves r

balanced :: Tree3 a -> Bool
balanced (Leaf3 _)   = True
balanced (Node3 l r) = abs (leaves l - leaves r) <= 1
                       && balanced l && balanced r

-- Zelftoets 3
data Punt a = Pt a a
   deriving (Show,Eq)

data Lijnstuk a = Lst (Punt a) (Punt a)
   deriving Show
   
instance Eq a => Eq (Lijnstuk a) where
   Lst p1 p2 == Lst p3 p4 = ((p1 == p3) && (p2 == p4))
                         || ((p1 == p4) && (p2 == p3))