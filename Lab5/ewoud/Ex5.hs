module Ex5 where

import Data.List
import System.Random
import Lecture5Refactor
--import Lecture5

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: xh

------------------------------------------------------------------------------}
-- refactor has 2 constrains
-- normal sudoku: gridConstr
-- nrc sudoku: nrcGridConstr

main5 = do
    --print("REFACTOR")
    --sud <- genRandomSudoku
    --problem <- genProblem sud
    --showNode problem
    --solveShowNs [problem] gridConstr
    -------------------------------
    print("NRC Refactor")
    nrcsud <- genRandomNrcSudoku
    nrcproblem <- genProblemNrc nrcsud
    showNode nrcproblem
    solveShowNs [nrcproblem] nrcGridConstr