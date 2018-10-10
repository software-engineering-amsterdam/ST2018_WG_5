module Assignment6 where

import Data.List
import System.Random
import Lecture6
import Assignment5
import Assignment4

{------------------------------------------------------------------------------

  Assignment 6.1

  Hours spent: 0.5
  Answer:
    The results look kind of similar between the composites and the carmichaels. This seems to
    work out with things I found on MR https://cs.stackexchange.com/questions/21462/why-miller-rabin-instead-of-fermat-primality-test

    It seems like MR has no hard numbers on which it fails, but the chances are equal for all composites.

------------------------------------------------------------------------------}
testMR k (x:xs) = do
                    result <- primeMR k x
                    if result then print ("Found a failure: " ++ (show x)) else testMR k xs

assignment61 = do
              print "Composites tests"
              testMR 1 composites
              testMR 2 composites
              testMR 3 composites
              print "Carmichael tests"
              testMR 1 carmichael
              testMR 2 carmichael
              testMR 3 carmichael
{- Output:
    "Composites tests"
    "Found a failure: 9"
    "Found a failure: 49141"
    "Found a failure: 736291"
    "Carmichael tests"
    "Found a failure: 294409"
    "Found a failure: 15181505298649"
    "Found a failure: 1550605263612818689"
-}

{------------------------------------------------------------------------------

  Assignment 6.2

  Hours spent:
  Answer:
    TODO

------------------------------------------------------------------------------}