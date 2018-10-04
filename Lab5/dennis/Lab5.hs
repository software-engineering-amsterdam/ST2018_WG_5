module Lab5 where

import Data.List
import System.Random
--import Lecture5
--import Lecture5Ass1
import Lecture5Ass2
import Control.Applicative


{------------------------------------------------------------------------------

    Assignment 1

    Hours spent: 1.5

------------------------------------------------------------------------------}
-- Sudoku from assignment1
sudoku1 :: Grid
sudoku1  = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

-- CODE IS IN Lecture5Ass1.hs

-- solveAndShow sudoku1
--  +-------+-------+-------+
--  | 4 7 8 | 3 9 2 | 6 1 5 |
--  | 6 1 9 | 7 5 8 | 3 2 4 |
--  | 2 3 5 | 4 1 6 | 9 7 8 |
--  +-------+-------+-------+
--  | 7 2 6 | 8 3 5 | 1 4 9 |
--  | 8 9 1 | 6 2 4 | 7 5 3 |
--  | 3 5 4 | 9 7 1 | 2 8 6 |
--  +-------+-------+-------+
--  | 5 6 7 | 2 8 9 | 4 3 1 |
--  | 9 8 3 | 1 4 7 | 5 6 2 |
--  | 1 4 2 | 5 6 3 | 8 9 7 |
--  +-------+-------+-------+
--  [()]

{------------------------------------------------------------------------------

    Assignment 2

    Hours spent: 0.75

    CODE IN Lecture5Ass2.hs

    - The refactored code is easier to modify for NRC sudokus due to the easier implementation of extra constraints.
    - Test for efficiency:

    > time ./test_non_rf
    real    0m0,263s
    user    0m0,256s
    sys     0m0,006s
    > time ./test_rf
    real    0m0,262s
    user    0m0,250s
    sys     0m0,013s

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 1

------------------------------------------------------------------------------}

checkIfMinimal :: Node -> Bool
checkIfMinimal n = all (\x -> removePosAndCheckUnique n x == False) (filledPositions (node2sud n))

removePosAndCheckUnique :: Node -> (Row, Column) -> Bool
removePosAndCheckUnique node pos = uniqueSol (eraseN node pos)

node2sud :: Node -> Sudoku
node2sud (s, _) = s

ass3 = do
    x <- genRandomSudoku
    y <- genProblem x
    print $ uniqueSol y
    print $ checkIfMinimal y

--  *Lab5> ass3
--  True
--  True

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 1

------------------------------------------------------------------------------}

--emptyBlock :: Sudoku ->
getBlockPositions :: [Int] -> [Int] -> [(Int, Int)]
getBlockPositions xs ys = (,) <$> ys <*> xs

eraseNodes :: Node -> [(Row, Column)] -> Node
eraseNodes n [] = n
eraseNodes n (xy:xs) = eraseNodes (eraseN n xy) xs

emptyBlock :: [Int] -> [Int] -> Node -> Node
emptyBlock xs ys n = eraseNodes n (getBlockPositions xs ys)

solveNode :: Node -> IO[()]
solveNode n = solveAndShow $ sud2grid $ node2sud n

ass4 = do
    -- Generate a sudoku problem with 3 empty blocks
    x1 <- genRandomSudoku
    y1 <- genProblem $ emptyBlock [1..3] [1..3] $ emptyBlock [4..6] [1..3] $ emptyBlock [7..9] [1..3] x1
    showNode y1
--    solveNode y1
    print "------------------------------------"
    -- Generate a sudoku problem with 4 empty blocks
    x2 <- genRandomSudoku
    y2 <- genProblem $ emptyBlock [1..3] [1..3] $ emptyBlock [4..6] [1..3] $ emptyBlock [7..9] [1..3] $ emptyBlock [1..3] [4..6] x2
    showNode y2
--    solveNode y2
    print "------------------------------------"
    -- Generate a sudoku problem with 5 empty blocks
    x3 <- genRandomSudoku
    y3 <- genProblem $ emptyBlock [1..3] [1..3] $ emptyBlock [4..6] [1..3] $ emptyBlock [7..9] [1..3] $ emptyBlock [1..3] [4..6]  $ emptyBlock [4..6] [4..6] x3
    showNode y3
--    solveNode y3

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent:

------------------------------------------------------------------------------}