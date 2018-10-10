module Assignment4 where

import Data.List
import System.Random
import Lecture6
import Assignment3

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 0.5
  Answer:
    So the Fermat's primality check fails at classifying composites as non prime. the method below
    will find such a composite which is listed as prime but in reality is not.

    It looks like that increasing K increases the correctness of primeTestsF, since the first false positive
    occurs later and thus more are correctly recognised as non prime.
------------------------------------------------------------------------------}

testF k (x:xs) = do
                    result <- primeTestsF k x
                    if result then print ("Found a failure: " ++ (show x)) else testF k xs

assignment4 = do
                testF 1 composites
                testF 2 composites
                testF 3 composites
                testF 4 composites
                testF 10 composites
{- Output:
    "Found a failure: 21"
    "Found a failure: 703"
    "Found a failure: 1541"
    "Found a failure: 2465"
    "Found a failure: 29341"
-}