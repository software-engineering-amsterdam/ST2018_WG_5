module Assignment5 where
import Lecture6
import Assignment4

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: 0.25
  Answer:
    The result is found below. No matter what K is the first failure is always the first in the list
    of carmichael's composite numbers.

    This is easy to explain since this list of carmichael is the exact counter for the Fermat theorem.
    On Wikipedia they call it the Fermat pseudoprimes for this reason. This means that every number in the list
    is set to fool the Fermat theorem and thus is the first in this list always the first counterexample.

------------------------------------------------------------------------------}
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

assignment5 = do
              testFermat 1 carmichael
              testFermat 2 carmichael
              testFermat 3 carmichael
              testFermat 4 carmichael
              testFermat 10 carmichael
{- Output:
    *Assignment5> assignment5
    "Found a failure: 294409"
    "Found a failure: 294409"
    "Found a failure: 294409"
    "Found a failure: 294409"
    "Found a failure: 294409"
-}