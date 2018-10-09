module Assignment where

import Data.List
import System.Random
import Lecture5Nrc

{------------------------------------------------------------------------------

    Assignment 1

    Hours spent: 1h

    State: Done

    CHANGELOG also in Lecture5Nrc (assignment1)
    All changes marked by EDIT
    added blocksNrc
    change prune
    added bl for nrc (blNrc)
    added grid check for nrc (subGridNrc)
    added free in grid check for nrc (freeInSubgridNrc)
    added sameblock for nrc (sameblockNrc)
    changed, added extra intersect operation for nrc grid (freeAtPos)
    added subgrid injective check for nrc (subgridInjectiveNrc)
    changed, added extra nrc subgrid injective step (consistent)

------------------------------------------------------------------------------}


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

assignment1 = solveAndShow sudoku1
{--
+-------+-------+-------+
| 4 7 8 | 3 9 2 | 6 1 5 |
| 6 1 9 | 7 5 8 | 3 2 4 |
| 2 3 5 | 4 1 6 | 9 7 8 |
+-------+-------+-------+
| 7 2 6 | 8 3 5 | 1 4 9 |
| 8 9 1 | 6 2 4 | 7 5 3 |
| 3 5 4 | 9 7 1 | 2 8 6 |
+-------+-------+-------+
| 5 6 7 | 2 8 9 | 4 3 1 |
| 9 8 3 | 1 4 7 | 5 6 2 |
| 1 4 2 | 5 6 3 | 8 9 7 |
+-------+-------+-------+
[()]
--}