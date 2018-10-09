module Assignment5 where

import RefactoredSource

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: 1
  Answer:
    I've traced back all generator methods just like the solve methods and implemented the
    constraints there as well.
    See below for some comparisons of the refactored and non refactored solution +
    using the refactored solution for generating NRC sudokus
------------------------------------------------------------------------------}
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
nrcBlockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]

assignment5 = do
                print("Original non refactored generation + solution")
                sud <- genRandomSudoku
                problem <- genProblem sud
                showNode problem
                solveShowNs [problem]
                print("Original refactored generation + solution")
                sudRef <- genRandomSudoku' (rowConstrnt ++ columnConstrnt ++ blockConstrnt)
                problemRef <- genProblem' sudRef (rowConstrnt ++ columnConstrnt ++ blockConstrnt)
                showNode problemRef
                solveShowNs [problemRef]
                print("NRC refactored generation + solution")
                sudNrc <- genRandomSudoku' (rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ nrcBlockConstrnt)
                problemNrc <- genProblem' sudNrc (rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ nrcBlockConstrnt)
                showNode problemNrc
                solveShowNs' [problemNrc] (rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ nrcBlockConstrnt)

{-
    "Original non refactored generation + solution"
    +-------+-------+-------+
    |       |   1   | 9 5   |
    | 4   2 |     8 |       |
    |     5 |       | 2     |
    +-------+-------+-------+
    |   2 6 |       |       |
    |   3 7 | 9     | 8     |
    |       |       |   7   |
    +-------+-------+-------+
    |       | 1 5   |     7 |
    |       |   7 6 |   1   |
    |   8   |       |   6   |
    +-------+-------+-------+
    +-------+-------+-------+
    | 6 7 3 | 4 1 2 | 9 5 8 |
    | 4 1 2 | 5 9 8 | 7 3 6 |
    | 8 9 5 | 6 3 7 | 2 4 1 |
    +-------+-------+-------+
    | 5 2 6 | 7 8 4 | 1 9 3 |
    | 1 3 7 | 9 6 5 | 8 2 4 |
    | 9 4 8 | 3 2 1 | 6 7 5 |
    +-------+-------+-------+
    | 2 6 4 | 1 5 9 | 3 8 7 |
    | 3 5 9 | 8 7 6 | 4 1 2 |
    | 7 8 1 | 2 4 3 | 5 6 9 |
    +-------+-------+-------+
    "Original refactored generation + solution"
    +-------+-------+-------+
    |   9   | 2     |       |
    |       |   5   |       |
    |   2 6 | 8     | 3     |
    +-------+-------+-------+
    |       | 9   4 |   3   |
    | 6 7 2 | 3 1   |   9   |
    |       |       |       |
    +-------+-------+-------+
    |   6   |   2   |   1 4 |
    | 1   4 |       |   7   |
    |     3 |       |     9 |
    +-------+-------+-------+
    +-------+-------+-------+
    | 4 9 1 | 2 3 6 | 5 8 7 |
    | 7 3 8 | 4 5 1 | 9 2 6 |
    | 5 2 6 | 8 7 9 | 3 4 1 |
    +-------+-------+-------+
    | 8 1 5 | 9 6 4 | 7 3 2 |
    | 6 7 2 | 3 1 5 | 4 9 8 |
    | 3 4 9 | 7 8 2 | 1 6 5 |
    +-------+-------+-------+
    | 9 6 7 | 5 2 3 | 8 1 4 |
    | 1 5 4 | 6 9 8 | 2 7 3 |
    | 2 8 3 | 1 4 7 | 6 5 9 |
    +-------+-------+-------+
    "NRC refactored generation + solution"
    +-------+-------+-------+
    |       | 4     | 8   1 |
    |       |   2 1 |   3   |
    |   2   |       |       |
    +-------+-------+-------+
    |       |   1   |       |
    |       |     6 |       |
    |       |       |       |
    +-------+-------+-------+
    | 4 8   |       | 5     |
    |       | 9 6 7 |       |
    |       |       | 3     |
    +-------+-------+-------+
    +-------+-------+-------+
    | 9 6 3 | 4 7 5 | 8 2 1 |
    | 5 4 8 | 6 2 1 | 7 3 9 |
    | 7 2 1 | 3 8 9 | 6 5 4 |
    +-------+-------+-------+
    | 6 5 9 | 7 1 8 | 2 4 3 |
    | 3 1 2 | 5 4 6 | 9 7 8 |
    | 8 7 4 | 2 9 3 | 1 6 5 |
    +-------+-------+-------+
    | 4 8 6 | 1 3 2 | 5 9 7 |
    | 1 3 5 | 9 6 7 | 4 8 2 |
    | 2 9 7 | 8 5 4 | 3 1 6 |
    +-------+-------+-------+
    [()]
-}