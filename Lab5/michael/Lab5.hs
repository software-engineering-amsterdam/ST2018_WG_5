module Lab5 where

-- import OriginalSource
import RefactoredSource
-- import NrcSource

{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: 2
  Answer:
    The implementation is found in NrcSource

    In order to fix this by adding code to the existing lecture code I had to add a few things.
    -   I added "[ nrcGridInjective s (r,c) | r <- [2,6], c <- [2,6]]" to the "consistent" function
        -   This is necessary  to make sure there is a check for invalid Nrc sudokus to begin with. This is required
            since it's now possible to have a solvable sudoku but an unsolvable Nrc sudoku
    -   I added "nrcBlocxks" which is just a way to harcode the nrc blocks in the same way "blocks" does
        for the original blocks
    -   I added the nrcBl function which returns in which nrc block the given index falls (2 -> [2,3,4])
    -   Added "freeInNrcGrid" which gets all possible inputs for a field by only taking the Nrc block rule
        into account
    -   Added   "| sameNrcBlock (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest" to the prune function
        which adds the nrc rule to the pruning process and makes sure no same digits are being placed in
        the same nrc block
------------------------------------------------------------------------------}
assignment1Test =[[0,0,0,3,0,0,0,0,0],
                  [0,0,0,7,0,0,3,0,0],
                  [2,0,0,0,0,0,0,0,8],
                  [0,0,6,0,0,5,0,0,0],
                  [0,9,1,6,0,0,0,0,0],
                  [3,0,0,0,7,1,2,0,0],
                  [0,0,0,0,0,0,0,3,1],
                  [0,8,0,0,4,0,0,0,0],
                  [0,0,2,0,0,0,0,0,0]]

assignment1 = do
    solveAndShow assignment1Test
{- Output: (after importing the NrcSource instead of another!!!!)
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
-}

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 3
  Answer:
    The implementation is found in RefactoredSource

    I traced back the working code and implemented the Constrnt parameter everywhere, this way everything
    is working like stated in the assignment. (the checking, generating options and consistent check)

    It's easy to see that this new way of implementing the sudoku is wayyy easier to extend as you can just
    add various constraints and it keeps working. It also performs a bit better since every constraint is
    checked at once instead in some arbitrary order one after another in as seen in the old solution.

    In the output you can see that the new way and old way still give the same result. Also using this new
    way for doing the NRC one also yields the same result. This confirms everything is still working.

    Also I've tested that inserting an invalid sudoku in all possible ways (same number in column, row, block and nrc block)
    will all result in an empty array because of the consistent check that's also implemented in this new way.
------------------------------------------------------------------------------}
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
nrcBlockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]

assignment2 = do
    print "Refactored way"
    solveAndShow' example1 (rowConstrnt ++ columnConstrnt ++ blockConstrnt)
    print "Original way"
    solveAndShow example1
    print "Refactored way NRC test"
    solveAndShow' assignment1Test (rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ nrcBlockConstrnt)
{-- Output: After using RefactoredSource as source!!!!
"Refactored way"
+-------+-------+-------+
| 5 3 4 | 6 7 8 | 9 1 2 |
| 6 7 2 | 1 9 5 | 3 4 8 |
| 1 9 8 | 3 4 2 | 5 6 7 |
+-------+-------+-------+
| 8 5 9 | 7 6 1 | 4 2 3 |
| 4 2 6 | 8 5 3 | 7 9 1 |
| 7 1 3 | 9 2 4 | 8 5 6 |
+-------+-------+-------+
| 9 6 1 | 5 3 7 | 2 8 4 |
| 2 8 7 | 4 1 9 | 6 3 5 |
| 3 4 5 | 2 8 6 | 1 7 9 |
+-------+-------+-------+
"Original way"
+-------+-------+-------+
| 5 3 4 | 6 7 8 | 9 1 2 |
| 6 7 2 | 1 9 5 | 3 4 8 |
| 1 9 8 | 3 4 2 | 5 6 7 |
+-------+-------+-------+
| 8 5 9 | 7 6 1 | 4 2 3 |
| 4 2 6 | 8 5 3 | 7 9 1 |
| 7 1 3 | 9 2 4 | 8 5 6 |
+-------+-------+-------+
| 9 6 1 | 5 3 7 | 2 8 4 |
| 2 8 7 | 4 1 9 | 6 3 5 |
| 3 4 5 | 2 8 6 | 1 7 9 |
+-------+-------+-------+
"Refactored way NRC test"
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
-}

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: -
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
{- Output: (Using RefactoredSource.hs!!!!!)
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