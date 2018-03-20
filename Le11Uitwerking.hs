------------------------------------------------------------
-- IB1602: Functioneel programmeren 
--    Open Universiteit Nederland 2017
-- 
-- Bouwsteen bij leereenheid 11: 
--    Unbeatable tic-tac-toe
------------------------------------------------------------
module Le11 where

import Le11Tictactoe hiding (play')
import System.Random hiding (next)

-- Opgave 1
data PlayerN = ON | XN
    deriving (Eq,Ord,Show)

type GridN = [[Maybe Player]]

nextN :: PlayerN -> PlayerN
nextN ON = XN
nextN XN = ON

-- Opgave 2
emptyN :: GridN
emptyN = replicate size (replicate size Nothing)

-- Opgave 3
nodes :: Tree a -> Int
nodes (Node _ ts) = 1 + sum (map nodes ts)

mydepth :: Tree a -> Int
mydepth (Node _ []) = 0
mydepth (Node _ ts) = 1 + maximum (map mydepth ts)

-- Zelftoets 1
bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g', p') _ <- ts, p' == best]
    where
        tree = prune depth (gametree g p)
        Node (_, best) ts = minimax tree

play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play' g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking... "
                   let gs = bestmoves g p
                   n <- randomRIO (0, length gs - 1)
                   play (gs !! n) (next p)

-- Zelftoets 2
mindepthtrees :: Tree Grid -> Tree (Grid,Integer)
mindepthtrees (Node g []) = Node (g, 0) []
mindepthtrees (Node g ts) = Node (g, 1 + minimum ds) ts'
                            where
                              ts'  = map mindepthtrees ts
                              ds   = [d | (Node (_, d) _) <- ts']

bestmoves' :: Grid -> Player -> [Grid]
bestmoves' g p = [g' | Node (g', d') _ <- ts, d' == best - 1]
                where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = mindepthtrees tree

bestmove' :: Grid -> Player -> Grid
bestmove' g p = head (bestmoves' g p)
