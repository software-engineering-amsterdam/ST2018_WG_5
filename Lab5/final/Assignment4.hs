module Assignment4 where

import Data.List
import System.Random
import Lecture5

import Control.Applicative

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 3
  Answer:

------------------------------------------------------------------------------}

node2sud :: Node -> Sudoku
node2sud (s, _) = s

getBlockPositions :: [Int] -> [Int] -> [(Int, Int)]
getBlockPositions xs ys = (,) <$> ys <*> xs

eraseNodes :: Node -> [(Row, Column)] -> Node
eraseNodes n [] = n
eraseNodes n (xy:xs) = eraseNodes (eraseN n xy) xs

emptyBlock :: [Int] -> [Int] -> Node -> Node
emptyBlock xs ys n = eraseNodes n (getBlockPositions xs ys)

solveNode :: Node -> IO[()]
solveNode n = solveAndShow . sud2grid $ node2sud n

assignment4 = do
    -- Generate a sudoku problem with 3 empty blocks
    x1 <- genRandomSudoku
    y1 <- genProblem . emptyBlock [1..3] [1..3] $ emptyBlock [4..6] [1..3] $ emptyBlock [7..9] [1..3] x1
    showNode y1
--    solveNode y1
    print "------------------------------------"
    -- Generate a sudoku problem with 4 empty blocks
    x2 <- genRandomSudoku
    y2 <- genProblem . emptyBlock [1..3] [1..3] $ emptyBlock [4..6] [1..3] $ emptyBlock [7..9] [1..3] $ emptyBlock [1..3] [4..6] x2
    showNode y2
--    solveNode y2
    print "------------------------------------"
    -- Generate a sudoku problem with 5 empty blocks
    x3 <- genRandomSudoku
    y3 <- genProblem . emptyBlock [1..3] [1..3] $ emptyBlock [4..6] [1..3] $ emptyBlock [7..9] [1..3] $ emptyBlock [1..3] [4..6]  $ emptyBlock [4..6] [4..6] x3
    showNode y3
--    solveNode y3