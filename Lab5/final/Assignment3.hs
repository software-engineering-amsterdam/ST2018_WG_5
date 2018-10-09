module Assignment3 where

import RefactoredSource

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 2
  Answer:
    2 things should be tested:
        -   If there is only one solution to the minimal sudoku
        -   If by removing ANY digit the solution options becomes > 1

    As can be seen this test succeeds. The test starts of with checking if it's really a one solution problem.
    Afterwards a list of positions is created with all positions containing data. Then there is a check which
    checks that after removing every individual position the solutions  become > 1.
------------------------------------------------------------------------------}
-- Returns a list of positions being filled
filleds :: Node -> [(Row, Column)]
filleds (sud, _) = filledPositions sud

-- Performs the remove -> check one solution check on all positions given
performCheck :: Node -> [(Row, Column)] -> Bool
performCheck _ [] = True
performCheck node positions = all (==False) (map (\pos -> removePosCheck node pos) positions)

-- Checks if the node is still a one solution node after removing the position
removePosCheck :: Node -> (Row, Column) -> Bool
removePosCheck node pos = uniqueSol (eraseN node pos)

assignment3 = do
                   r <- genRandomSudoku
                   s <- genProblem r
                   print "Only one solution check:"
                   print (uniqueSol s)
                   print "Removing anything results in more then one solution check:"
                   let filledPositions = filleds s
                   print "Filled positions to check"
                   print (filledPositions)
                   print "Performing Check.."
                   print (performCheck s filledPositions)
                   showNode s
{- Output:
    "Only one solution check:"
    True
    "Removing anything results in more then one solution check:"
    "Filled positions to check"
    [(1,8),(2,1),(2,2),(2,5),(2,9),(3,3),(3,4),(3,5),(4,1),(4,4),(4,5),(4,9),(5,3),(5,6),(6,2),(6,3),(7,3),(7,7),(8,2),(8,4),(8,5),(8,7),(9,1),(9,4),(9,5)]
    "Performing Check.."
    True
    +-------+-------+-------+
    |       |       |   5   |
    | 6 2   |   3   |     8 |
    |     8 | 1 4   |       |
    +-------+-------+-------+
    | 5     | 8 1   |     9 |
    |     7 |     2 |       |
    |   9 1 |       |       |
    +-------+-------+-------+
    |     9 |       | 3     |
    |   1   | 9 2   | 8     |
    | 4     | 5 8   |       |
    +-------+-------+-------+
-}