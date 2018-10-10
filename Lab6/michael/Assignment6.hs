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
    For this excercise I will try to find the first 15 mersenne primes using the method described
    in the exercise. Trying out k = 1 and k = 10 to see if that makes a difference.

    A list found on the internet of the first 15 mersenne primes looks like this:
    2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279
    (https://en.wikipedia.org/wiki/Mersenne_prime)

    They seem to match with what I've found regardless of K. K only seems to have influences on the
    execution time it takes to come up with the numbers

------------------------------------------------------------------------------}

toMersenne x = (2^x) - 1
mersenneIsMRPrime k x = primeMR k (toMersenne x)

assignment62 0 k _ = print "done"
assignment62 n k (x:xs) = do
                    result <- mersenneIsMRPrime k x
                    if result then do
                                print (show x)
                                assignment62 (n - 1) k xs
                            else assignment62 n k xs

assignment62k1 = do
                    assignment62 15 1 primes
{- Output:
    *Assignment6> assignment62k1
    "2"
    "3"
    "5"
    "7"
    "13"
    "17"
    "19"
    "31"
    "61"
    "89"
    "107"
    "127"
    "521"
    "607"
    "1279"
    "done"
    (0.28 secs, 213,981,528 bytes)
-}

assignment62k10 = do
                    assignment62 15 10 primes
{- Output:
    *Assignment6> assignment62k10
    "2"
    "3"
    "5"
    "7"
    "13"
    "17"
    "19"
    "31"
    "61"
    "89"
    "107"
    "127"
    "521"
    "607"
    "1279"
    "done"
    (0.47 secs, 356,854,320 bytes)
-}
