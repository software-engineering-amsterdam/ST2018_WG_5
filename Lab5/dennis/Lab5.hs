module Lab5 where

import Data.List
import System.Random
--import Lecture5
import Lecture5
import Debug.Trace

{------------------------------------------------------------------------------

    Assignment 1

    Hours spent: 1h

    Answer:


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

-- CODE IS IN Lecture5.hs

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