module Lab5 where

import Data.List
import System.Random
--import Lecture5
import Lecture5Refactor


{------------------------------------------------------------------------------

    Assignment 1

    Hours spent: 1h

    State: 
    I think it works?
    Todo: 
    - check a few solutions by hand
    - add 2 grids overlay to print (COSMETIC)
    - better solution display (COSMETIC)

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

-- Place 2 grids over the 3x3 blocks grid over
-- (2,2), (2,6), (6,2), and (6,6).
blocksExtra :: [[Int]]
blocksExtra = [[2..4],[6..8]]

-- IDEA
-- Start from solveAndShow :: Grid -> IO[()] and work backwards!
-- we maken er een puzzle van 

-- Changed: 
-- solveShowNs -> solveShowNs'
solveAndShow' :: Grid -> IO[()]
solveAndShow' gr = solveShowNs' (initNode gr)

-- Changed:
-- solveNs -> solveNs'
solveShowNs' :: [Node] -> IO[()]
solveShowNs' = sequence . fmap showNode . solveNs

-- Changed:
-- succNode -> succNode'
solveNs' :: [Node] -> [Node]
solveNs' = search succNode' solved 

-- Changed:
-- extendNode -> extendNode'
succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p

-- Changed:
-- prune -> prune'
extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

-- Changed:
-- prune -> prune'
-- added sameblock (sameblock') condition for the extra blacks
prune' :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | sameblock (r,c) (x,y) = (x,y,zs\\[v]) : prune' (r,c,v) rest
    -- Extra condition for the NRC blocks
    | sameblock' (r,c) (x,y) = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | otherwise = (x,y,zs) : prune' (r,c,v) rest

-- Changed:
-- bl -> bl'
sameblock' :: (Row,Column) -> (Row,Column) -> Bool
sameblock' (r,c) (x,y) = bl' r == bl' x && bl' c == bl' y 

-- Changed:
-- blocks -> blocksExtra
bl' :: Int -> [Int]
bl' x = concat $ filter (elem x) blocksExtra

-- Think it works, loads of combinations?
assignment1 = solveAndShow' sudoku1

{------------------------------------------------------------------------------

    Assignment 2

    Hours spent: xh

    State:
    Added the refactors from the assignment.
    Works by changing import Lecture5 to import Lecture5Refactor

    Todo:
    - Check for further improvements
    - Test improvement (benchmark?)
    - Add NRC changes from Assignment1

------------------------------------------------------------------------------}

-- Check Lecture5Refactor.hs

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: xh

------------------------------------------------------------------------------}
-- Code from Lecture5 code:
--
-- uniqueSol 
-- eraseN
-- minimalize

-- IDEA:
-- find unique sodokuA  (uniqueSol)
--      remove node 1 (the "hint"?) from unique sodokuA
--      check if sodokuA - node 1 is not unique
--      repeat for all node n

-- wiki defi:
-- (Sudokus in which no clue can be deleted without losing uniqueness of the solution) 

-- CONFUSION
-- Only need to check P minus 1 hint. So first remove hint 1, then put back hint 1 and remove hint 2 and so on??
{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: xh

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: xh

------------------------------------------------------------------------------}