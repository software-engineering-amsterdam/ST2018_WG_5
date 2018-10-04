module Assignment1 where

import NrcSource
import Debug.Trace

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
{- Output:
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