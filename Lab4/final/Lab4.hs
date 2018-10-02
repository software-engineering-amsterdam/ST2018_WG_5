module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: TODO
  Answer:
    - TODO

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: TODO

------------------------------------------------------------------------------}

--TODO

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: TODO

------------------------------------------------------------------------------}

--TODO

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent:
  Answer:
    - TODO

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: TODO

------------------------------------------------------------------------------}

--TODO

{------------------------------------------------------------------------------

  Assignment 6

  Hours spent: TODO
  Answer:

------------------------------------------------------------------------------}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--TODO

{------------------------------------------------------------------------------

  Assignment 7

  Hours spent: TODO
  Answer:
    -   TODO

------------------------------------------------------------------------------}

--TODO

{------------------------------------------------------------------------------

  Assignment 8

  Hours spent: 0.25
  Answer:
    This is not true, the order of applying the 2 operations DOES matter. The main point why it does is
    that transitivity is also true whenever 2 pairs have nothing to do with each other. When symmetry is
    added first this causes the situation to change completely.
    Example:
    sym -> trans
    {(1,3),(2,4)} -> {(1,3),(3,1),(2,4),(4,2)} -> {(1,3),(3,1),(2,4),(4,2),(1,1),(3,3),(2,2),(4,4)}

    trans -> sym
    {(1,3),(2,4)} -> {(1,3),(2,4)} -> {(1,3),(3,1),(2,4),(4,2)}

    As you can see the result is different when the order of applying the operations is swapped.

------------------------------------------------------------------------------}

