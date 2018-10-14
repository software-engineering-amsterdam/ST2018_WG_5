module Lab6 where

import Data.List
import System.Random
import Numeric
import Data.Bits
import Lecture6 (prime, primeTestsF, primeMR, primes, 
    -- Assignment 6_2
    mers, 
    -- Assignment 7
    rsaPublic, rsaPrivate, rsaEncode, rsaDecode)


{------------------------------------------------------------------------------

Assignment 1

Hours spent: 1h

Idea: Left-to-right binary method
Source: https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method
------------------------------------------------------------------------------}

-- f :: base(b) -> exponent(e) -> modulus(m) ->  return  result
exM :: Integer -> Integer -> Integer -> Integer
exM b 0 m = 1
exM b e m = mult * exM ((b * b) `mod` m) (shiftR e 1) m `mod` m where
    -- if bit value [,,xi,,] is 0, skip to next step.
    -- otherwise compute result
    mult    | testBit e 0 = b `mod` m
            | otherwise = 1


{------------------------------------------------------------------------------

Assignment 2

Hours spent: 1h

See bench2.html for more results.

Increasing the exponent
original expm:
10^10 mod 123 : mean 245.8 ns   (243.7 ns .. 250.3 ns)
10^100 mod 123 : mean 522.4 ns   (521.3 ns .. 523.3 ns)
10^1000 mod 123 : mean 1.608 μs   (1.599 μs .. 1.630 μs)
10^10000 mod 123 : mean 29.79 μs   (29.73 μs .. 29.96 μs)

custom exM
10^10 mod 123 : mean 382.4 ns   (382.0 ns .. 383.2 ns)
10^100 mod 123 : mean 639.6 ns   (631.9 ns .. 668.2 ns
10^1000 mod 123 : mean 1.074 μs   (1.048 μs .. 1.114 μs)
10^10000 mod 123 : mean 1.253 μs   (1.242 μs .. 1.275 μs)

The exM implementation scales much better with an increasing exponent compared 
to the original implementation.

Increasing the base
original expm
10^10 mod 123 : mean 246.7 ns   (245.8 ns .. 248.7 ns)
100^10 mod 123 : mean 255.3 ns   (251.3 ns .. 265.2 ns)
1000^10 mod 123 : mean 271.6 ns   (270.2 ns .. 274.8 ns)
10000^10 mod 123 : mean 296.8 ns   (295.3 ns .. 300.3 ns)

custom expm
10^10 mod 123 : mean 404.5 ns   (392.7 ns .. 423.5 ns)
100^10 mod 123 : mean 378.3 ns   (377.6 ns .. 379.0 ns)
1000^10 mod 123 : mean 383.1 ns   (380.9 ns .. 387.9 ns)
10000^10 mod 123 : mean 392.6 ns   (386.3 ns .. 405.7 ns)

Both implementations show similar performance with an increased base
------------------------------------------------------------------------------}

-- See: bench2.hs + bench2.html

{------------------------------------------------------------------------------

Assignment 3

Hours spent: 0.25h

We generate the compositis based on two conditions for n:
- n is a positive integer with n>1.
- n is not a prime number.
------------------------------------------------------------------------------}

-- f :: returns [composites]
composites :: [Integer]
composites = filter (\x -> not (prime x)) [2..]

{------------------------------------------------------------------------------

Assignment 4

Hours spent: 1h

Result:
k = 1, 100 tests, least composite number : 9
k = 2, 100 tests, least composite number : 9
k = 3, 100 tests, least composite number : 35
k = 5, 100 tests, least composite number : 91
k = 10, 100 tests, least composite number : 1105

By choosing a random value as the base there is a >50% chance that the number
is not a pseudoprime to the base.
By choosing k random values there is a 0.5^k possibility of not being fooled
by composite numbers.

------------------------------------------------------------------------------}

-- Checks if the value k is a Fermat liar.
-- The supplied array contains composites only.
--
-- f :: k -> [composites] -> return primality check
fermatLiar :: Int -> [Integer] -> IO Integer
fermatLiar k (a:as) = do
    liar <- primeTestsF k a
    if (liar)
        then return a
        else fermatLiar k as

-- Returns the smallest fermat liar for the supplied k value
-- after n tests.
--
-- f :: k -> [composites] -> cur attempt -> max attempts -> [fermatLiars] -> return smallest fermat liar
leastCompositeNum :: Int -> [Integer] -> Int -> Int -> [Integer] -> IO Integer
leastCompositeNum k as n s xs = 
    if (n == s)
        then return $ minimum xs
        else do
            liar <- fermatLiar k as
            leastCompositeNum k as (n+1) s $ xs ++ [liar]

ass4 = do
    let max = 100
    k1 <- leastCompositeNum 1 composites 0 max []
    print ("k = 1, " ++ show max ++ " tests, least composite number : " ++ show k1)
    k2 <- leastCompositeNum 2 composites 0 max []
    print ("k = 2, " ++ show max ++ " tests, least composite number : " ++ show k2)
    k3 <- leastCompositeNum 3 composites 0 max []
    print ("k = 3, " ++ show max ++ " tests, least composite number : " ++ show k3)
    k5 <- leastCompositeNum 4 composites 0 max []
    print ("k = 5, " ++ show max ++ " tests, least composite number : " ++ show k5)
    k10 <- leastCompositeNum 10 composites 0 max []
    print ("k = 10, " ++ show max ++ " tests, least composite number : " ++ show k10)

{------------------------------------------------------------------------------

Assignment 5

Hours spent: 1h

A carmichael number is an odd positive composite number that satisfies fermats
little theorem.

Result:

"k = 1, first 15 Carmichael numbers, avg of 500 tests. Percentage of false positives : 99.43995"
"k = 2, first 15 Carmichael numbers, avg of 500 tests. Percentage of false positives : 98.639885"
"k = 3, first 15 Carmichael numbers, avg of 500 tests. Percentage of false positives : 98.27986"
"k = 5, first 15 Carmichael numbers, avg of 500 tests. Percentage of false positives : 96.94645"
"k = 10, first 15 Carmichael numbers, avg of 500 tests. Percentage of false positives : 94.413086"
"k = 100, first 15 Carmichael numbers, avg of 500 tests. Percentage of false positives : 69.08004"
------------------------------------------------------------------------------}

-- f :: returns [carmichael numbers]
carmichael :: [Integer]
carmichael = 
    [ (6*k+1)*(12*k+1)*(18*k+1) |
        k <- [2..],
        prime (6*k+1),
        prime (12*k+1),
        prime (18*k+1) ]

-- Check if the carmichael number 
-- passes the primeTest test
--
-- f :: k -> carmichael number -> return primality check
fermatCarMichael :: Int -> Integer -> IO Integer
fermatCarMichael k a = do
    liar <- primeTestsF k a
    if (liar)
        then return 1
        else return 0

-- Check for the first n carmichael numbers if they
-- can pass the primeTest.
-- Function returns the false positive percentage. 
--
-- f :: k -> [carmichael] -> cur attempt -> max attempts -> running [result] -> returns false positive %
fermatCarMichaelN :: Int -> [Integer] -> Int -> Int -> [Integer] -> IO Float
fermatCarMichaelN k (a:as) n s xs = 
    if (n == s)
        then return $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)
        else do
            fooled <- fermatCarMichael k a
            fermatCarMichaelN k as (n+1) s $ xs ++ [fooled]

-- Returns the average false positive percentage of a
-- fermatCarMichaelN k check.
--
-- f :: k -> current Iter -> stop Iter -> running value -> first X numbers -> returns avg false positive %
avgCarMichael :: Int -> Int -> Int -> Float -> Int -> IO Float
avgCarMichael k n s x upTo =
    if (n == s)
        then return $ 100 * (x / (fromIntegral s))
        else do
            res <- fermatCarMichaelN k carmichael 0 upTo []
            avgCarMichael k (n + 1) s (x + res) upTo

ass5 = do
    -- iterates the first x carMichael numbers
    let upTo = 15
    let tries = 500 -- 500
    k1 <- avgCarMichael 1 0 tries 0.0 upTo
    print ("k = 1, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k1)
    k2 <- avgCarMichael 2 0 tries 0.0 upTo
    print ("k = 2, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k2)
    k3 <- avgCarMichael 3 0 tries 0.0 upTo
    print ("k = 3, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k3)
    k5 <- avgCarMichael 5 0 tries 0.0 upTo
    print ("k = 5, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k5)
    k10 <- avgCarMichael 10 0 tries 0.0 upTo
    print ("k = 10, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k10)
    k100 <- avgCarMichael 100 0 tries 0.0 upTo
    print ("k = 100, first " ++ show upTo ++ " Carmichael numbers, avg of " ++ show tries ++
        " tests. Percentage of false positives : " ++ show k100)

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
    let tries = 500 -- 500
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

{------------------------------------------------------------------------------

Assignment 7

Hours spent: 1h

State: working. Todo: add better bitlength check + message serialization?
------------------------------------------------------------------------------}
genNBitPrime :: Int -> IO (Integer)
genNBitPrime n = do
    -- stay in range
    p <- randomRIO((2 ^ (n - 1)) + 1, (2 ^ n) - 1)
    isPrime <- primeMR 10 p
    if isPrime 
        then return p
        else genNBitPrime n

rsaPrimePair :: Int -> IO (Integer, Integer)
rsaPrimePair n = do
    p <- genNBitPrime n
    q <- genNBitPrime n
    -- check iff unique
    if (p /= q)
        then return (p, q)
        else rsaPrimePair n

ass7 = do
    let nbits = 128
    -- Find 2 random and distinct prime numbers p and q.
    pq <- rsaPrimePair nbits
    let publicKey = rsaPublic (fst pq) (snd pq)
    let privateKey = rsaPrivate (fst pq) (snd pq)
    -- message
    -- perhaps add hash/seriliaztion to bypass Integer limitation
    let msg = 123456789
    print ("Original message: " ++ show msg)
    let msgEncode = rsaEncode publicKey msg
    print ("Encoded message: " ++ show msgEncode)
    let msgDecode = rsaDecode privateKey msgEncode
    print ("Decoded message: " ++ show msgDecode)