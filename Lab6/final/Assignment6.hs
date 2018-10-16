module Assignment6 where

import Data.List
import System.Random
import Lecture6
import Assignment5
import Assignment4

{------------------------------------------------------------------------------

Assignment 6_1

Hours spent: 0.5h

Using the miller rabin check instead of Fermat's primality rerunning exercise 5 gives
the following results:

"k = 1, first 500 Carmichael numbers, avg of 10 tests. Percentage of false positives : 10.4"
"k = 2, first 500 Carmichael numbers, avg of 10 tests. Percentage of false positives : 1.0"
"k = 3, first 500 Carmichael numbers, avg of 10 tests. Percentage of false positives : 0.18"
"k = 5, first 500 Carmichael numbers, avg of 10 tests. Percentage of false positives : 0.0"
"k = 10, first 500 Carmichael numbers, avg of 10 tests. Percentage of false positives : 0.0"
"k = 100, first 500 Carmichael numbers, avg of 10 tests. Percentage of false positives : 0.0"

The results show that the miller rabin check is much more accurate for a low k and with k > 4
the check performs reliable with no false positives.

This is because Rabin-miller extends  Fermat's primality check by checking if the number
has exactly two trivial square roots of unity modules n.
------------------------------------------------------------------------------}

-- Check if the carmichael number
-- passes the Miller-Rabin test
--
-- f :: k -> carmichael number -> return primality check
mrCarMichael :: Int -> Integer -> IO Integer
mrCarMichael k a = do
    liar <- primeMR k a
    if (liar)
        then return 1
        else return 0

-- Check for the first n carmichael numbers if they
-- can pass the Miller-Rabin test.
-- Function returns the false positive percentage.
--
-- f :: k -> [carmichael] -> cur attempt -> max attempts -> running [result] -> returns false positive %
mrCarMichaelN :: Int -> [Integer] -> Int -> Int -> [Integer] -> IO Float
mrCarMichaelN k (a:as) n s xs =
    if (n == s)
        then return $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)
        else do
            fooled <- mrCarMichael k a
            mrCarMichaelN k as (n+1) s $ xs ++ [fooled]

-- Returns the average false positive percentage of a
-- Miller-Rabin check.
--
-- f :: k -> current Iter -> stop Iter -> running value -> first X numbers -> returns avg false positive %
avgMillerRabin :: Int -> Int -> Int -> Float -> Int -> IO Float
avgMillerRabin k n s x upTo =
    if (n == s)
        then return $ 100 * (x / (fromIntegral s))
        else do
            res <- mrCarMichaelN k carmichael 0 upTo []
            avgMillerRabin k (n + 1) s (x + res) upTo

ass6a = do
    -- iterates the first x carMichael numbers
    let upTo = 500
    let tries = 100 -- 500
    k1 <- avgMillerRabin 1 0 tries 0.0 upTo
    print ("k = 1, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k1)
    k2 <- avgMillerRabin 2 0 tries 0.0 upTo
    print ("k = 2, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k2)
    k3 <- avgMillerRabin 3 0 tries 0.0 upTo
    print ("k = 3, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k3)
    k5 <- avgMillerRabin 5 0 tries 0.0 upTo
    print ("k = 5, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k5)
    k10 <- avgMillerRabin 10 0 tries 0.0 upTo
    print ("k = 10, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k10)
    k100 <- avgMillerRabin 100 0 tries 0.0 upTo
    print ("k = 100, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k10)

{------------------------------------------------------------------------------

Assignment 6_2

Hours spent: 1h

"k = 1, first 15 Mersenne primes, avg of 5 tests. Sorted by small to large  : 80.0%, results contained only Mersenne primes 80.0%"
"k = 2, first 15 Mersenne primes, avg of 5 tests. Sorted by small to large  : 100.0%, results contained only Mersenne primes 100.0%"
"k = 5, first 15 Mersenne primes, avg of 5 tests. Sorted by small to large  : 100.0%, results contained only Mersenne primes 100.0%"

A k = 2 is sufficient to discover the first 15 Mersenne primes reliably using
the Miller-Rabin primality check
------------------------------------------------------------------------------}

-- Returns the first n mersenne numbers
-- in a list.
-- Goes upto mersenne number 25 (2^21701-1)
-- Warning, very slow for n > 10
--
-- f :: n -> returns [n mersenne primes]
mers2List :: Integer -> [Integer]
mers2List 0 = []
mers2List n =  mers2List (n - 1) ++ [mers n]

-- Function that discovers mersenne numbers using
-- the Miller-Rabin primality check.
-- If a prime p passes the Miller check with 2 ^ p - 1
-- p is a mersenne nummber
--
-- f :: k -> n results -> [primes i] -> running results -> returning [n results]
discoverMersenneN :: Int -> Int -> Int -> [Integer] -> IO [Integer]
discoverMersenneN k n i xs =
    if (n == (length xs))
        then return xs
        else do
            let p = (primes !! i)
            let mp = 2 ^ p - 1
            isPrime <- primeMR k mp
            if isPrime
                then discoverMersenneN k n (i + 1) $ xs ++ [mp]
                else discoverMersenneN k n (i + 1) xs

-- Returns the average accuracy for the the Miller-Rabin
-- primality check to discover mersenne numbers.
--
-- Checks two things:
-- If Miller-Rabin can generate the first n Mersenne primes in the correct order with given k.
-- If Miller-Rabin only generates Mersenne primes with given k.
--
-- f :: k -> current Iter -> stop Iter -> [Mersennes] -> n results -> running result -> running result -> avg (order, correct)
avgDiscoverMersenneN :: Int -> Int -> Int -> [Integer] -> Int -> Int -> Int -> IO (Float, Float)
avgDiscoverMersenneN k n s ms upTo os cs =
    if (n == s)
        then return (((fromIntegral cs / fromIntegral s) * 100), (fromIntegral os / fromIntegral s) * 100)
        else do
            m <- discoverMersenneN k upTo 0 []
            -- check if the mersenne list is ordered by small to big
            let osi = fromEnum (m == ms)
            -- check if all mersenne primes are correct
            let csi = fromEnum (m == (intersect m ms))
            avgDiscoverMersenneN k (n + 1) s ms upTo (os + osi) (cs + csi)

-- Find the first 15 mersenne exponents
ass6b = do
    let upTo = 15
    let mersList = mers2List $ toInteger upTo
    let tries = 5
    m1 <- avgDiscoverMersenneN 1 0 tries mersList upTo 0 0
    print ("k = 1, first " ++ show upTo ++ " Mersenne primes, avg of " ++ show tries ++
        " tests. Sorted by small to large  : " ++ (show (fst m1)) ++
        "%, results contained only Mersenne primes " ++ (show (snd m1)) ++ "%")
    m2 <- avgDiscoverMersenneN 2 0 tries mersList upTo 0 0
    print ("k = 2, first " ++ show upTo ++ " Mersenne primes, avg of " ++ show tries ++
        " tests. Sorted by small to large  : " ++ (show (fst m2)) ++
        "%, results contained only Mersenne primes " ++ (show (snd m2)) ++ "%")
    m5 <- avgDiscoverMersenneN 5 0 tries mersList upTo 0 0
    print ("k = 5, first " ++ show upTo ++ " Mersenne primes, avg of " ++ show tries ++
        " tests. Sorted by small to large  : " ++ (show (fst m5)) ++
        "%, results contained only Mersenne primes " ++ (show (snd m5)) ++ "%")