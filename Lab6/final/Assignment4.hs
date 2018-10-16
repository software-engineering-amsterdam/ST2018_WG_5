module Assignment4 where
import Lecture6
import Assignment3

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 1
  Answer:
  As k increases the found composites are larger.
------------------------------------------------------------------------------}

testFermat k (x:xs) = do
   failureFound <- primeTestsF k x
   if failureFound then print $ x else testFermat k xs

ass4 = do
      testFermat 1 composites'
      testFermat 2 composites'
      testFermat 3 composites'
      testFermat 4 composites'
      testFermat 5 composites'
{- Output:

    *Assignment4> ass4
    9
    21
    91
    2465
    2821


-}