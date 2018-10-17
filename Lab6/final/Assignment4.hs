module Assignment4 where
import Lecture6
import Assignment3

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 1
  Answer:
  As k increases the found composites are larger.
  
  This is because for a sample of k possible fermat witnesses the possibility
  of all of them being fermat liars is LEQ 2^-k probability.
  Give a large enough k this probability approximates zero.
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