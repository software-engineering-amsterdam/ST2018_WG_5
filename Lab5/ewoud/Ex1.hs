module Ex1 where

import Data.List
import System.Random
import Lecture5Nrc

{------------------------------------------------------------------------------

    Assignment 1

    Hours spent: 1h

    State: Done

    See CHANGELOG in Lecture5Nrc.hs

    Todo: 
    - add 2 grids overlay to print (COSMETIC)
    - make the refactor pretty (a bit hacky atm)

------------------------------------------------------------------------------}

-- See CHANGELOG in Lecture5Nrc.hs

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