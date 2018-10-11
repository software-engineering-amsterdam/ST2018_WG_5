module Lab6 where

import Data.List
import System.Random
import Numeric
import Data.Bits
-- 
import Lecture6 (prime, primeTestsF, primeMR, primes)
-- Bonus assignment imports
import Lecture6 (rsaPublic, rsaPrivate, rsaEncode, rsaDecode)
import Data.Hashable


{------------------------------------------------------------------------------

Assignment 1

Hours spent: 1h

Idea: Right-to-left binary method aka modular exponentiation with repeated squaring
(https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method)
(https://nptel.ac.in/courses/106103015/10)
------------------------------------------------------------------------------}

-- Exm:: base(b) -> exponent(e) -> modulus(m) -> result
exM :: Integer -> Integer -> Integer -> Integer
exM b 0 m = 1
exM b e m = mult * exM ((b * b) `mod` m) (shiftR e 1) m `mod` m where
    -- if bit value [,,x,,] is 0, do not modify result
    -- otherwise set result
    mult    | testBit e 0 = b `mod` m
            | otherwise = 1


{------------------------------------------------------------------------------

Assignment 2

Hours spent: 1h

------------------------------------------------------------------------------}

-- See: bench2.hs + bench2.html

{------------------------------------------------------------------------------

Assignment 3

Hours spent: 0.25h

------------------------------------------------------------------------------}

-- A composite number n is a positive integer n>1 which is not prime (i.e., which has factors other than 1 and itself). 
-- The first few composite numbers (sometimes called "composites" for short) are 4, 6, 8, 9, 10, 12, 14, 15, 16, ... 
-- source: http://mathworld.wolfram.com/CompositeNumber.html

composites :: [Integer]
composites = filter (\x -> not (prime x)) [2..]

{------------------------------------------------------------------------------

Assignment 4

Hours spent: 1h

------------------------------------------------------------------------------}

-- Kleine uitleg: https://www.khanacademy.org/computing/computer-science/cryptography/random-algorithms-probability/v/fermat-primality-test-prime-adventure-part-10

-- Checks if the value k is a Fermat liar
-- The supplied array as contains composites only
fermatLiar :: Int -> [Integer] -> IO Integer
fermatLiar k (a:as) = do
    liar <- primeTestsF k a
    if (liar)
        then return a
        else fermatLiar k as

-- Finds the smallest fermat liar for the supplied k value.
-- Returns the smallest value after n random tests.
--
-- leastCompositeNum k -> composites -> current attempt -> max attempts -> fermatLiar results -> smallest fermat liar
leastCompositeNum :: Int -> [Integer] -> Int -> Int -> [Integer] -> IO Integer
leastCompositeNum k as n s xs = 
    if (n == s)
        then return $ minimum xs
        else do
            liar <- fermatLiar k as
            --leastCompositeNum k as (n+1) xs
            leastCompositeNum k as (n+1) s $ xs ++ [liar]

ass4 = do
    let max = 50
    let start = 0
    --ki <- leastCompositeNum i composites start max []
    --print ("k = i, " ++ show max ++ " tests, smallest least composite number : " ++ show ki)
    k1 <- leastCompositeNum 1 composites start max []
    print ("k = 1, " ++ show max ++ " tests, smallest least composite number : " ++ show k1)
    k2 <- leastCompositeNum 2 composites start max []
    print ("k = 2, " ++ show max ++ " tests, smallest least composite number : " ++ show k2)
    k3 <- leastCompositeNum 3 composites start max []
    print ("k = 3, " ++ show max ++ " tests, smallest least composite number : " ++ show k3)
    k5 <- leastCompositeNum 4 composites start max []
    print ("k = 5, " ++ show max ++ " tests, smallest least composite number : " ++ show k5)
    k10 <- leastCompositeNum 10 composites start max []
    print ("k = 10, " ++ show max ++ " tests, smallest least composite number : " ++ show k10)

{------------------------------------------------------------------------------

Assignment 5

Hours spent: 1h

------------------------------------------------------------------------------}

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
-- fermatCarMichael k, carmichael number
fermatCarMichael :: Int -> Integer -> IO Integer
fermatCarMichael k a = do
    liar <- primeTestsF k a
    if (liar)
        then return 1
        else return 0

-- Check for the first n carmichael numbers if they
-- can pass the primeTest. 
--
-- fermatCarMichaelN k, numbers, start, end, result
fermatCarMichaelN :: Int -> [Integer] -> Int -> Int -> [Integer] -> IO Float
fermatCarMichaelN k (a:as) n s xs = 
    if (n == s)
        then return $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)
        else do
            fooled <- fermatCarMichael k a
            fermatCarMichaelN k as (n+1) s $ xs ++ [fooled]

-- Check to see the amount of times we can generate
-- absolute Fermat pseudoprimes.
--
-- avgCarMichael :: k -> current Iter -> stop Iter -> running value -> first X numbers 
avgCarMichael :: Int -> Int -> Int -> Float -> Int -> IO Float
avgCarMichael k n s x upTo =
    if (n == s)
        then return $ 100 * (x / (fromIntegral s))
        else do
            res <- fermatCarMichaelN k carmichael 0 upTo []
            avgCarMichael k (n + 1) s (x + res) upTo

-- 
ass5 = do
     -- iterates the first x carMichael numbers
    let upTo = 500
    let tries = 10 -- 500
    k1 <- avgCarMichael 1 0 tries 0.0 upTo
    print ("k = 1, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k1)
    k2 <- avgCarMichael 2 0 tries 0.0 upTo
    print ("k = 2, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k2)
    k3 <- avgCarMichael 3 0 tries 0.0 upTo
    print ("k = 3, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k3)
    k5 <- avgCarMichael 5 0 tries 0.0 upTo
    print ("k = 5, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k5)
    k10 <- avgCarMichael 10 0 tries 0.0 upTo
    print ("k = 10, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k10)

{------------------------------------------------------------------------------

Assignment 6_1

Hours spent: 0.5h

Answer: results much more better because blablaba (compare ass5 versus ass6a)
------------------------------------------------------------------------------}
-- Check if the carmichael number 
-- passes the Miller-Rabin test
--
-- mrCarMichael k, carmichael number
mrCarMichael :: Int -> Integer -> IO Integer
mrCarMichael k a = do
    liar <- primeMR k a
    if (liar)
        then return 1
        else return 0

-- Check for the first n carmichael numbers if they
-- can pass the Miller-Rabin test. 
--
-- mrCarMichaelN k, numbers, start, end, result
mrCarMichaelN :: Int -> [Integer] -> Int -> Int -> [Integer] -> IO Float
mrCarMichaelN k (a:as) n s xs = 
    if (n == s)
        then return $ (fromIntegral $ sum xs) / (fromIntegral $ length xs)
        else do
            fooled <- mrCarMichael k a
            mrCarMichaelN k as (n+1) s $ xs ++ [fooled]

-- Check to see the amount of times we can generate
-- absolute Fermat pseudoprimes.
--
-- avgMillerRabin :: k -> current Iter -> stop Iter -> running value -> first X numbers 
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
    let tries = 10 -- 500
    k1 <- avgMillerRabin 1 0 tries 0.0 upTo
    print ("k = 1, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k1)
    k2 <- avgMillerRabin 2 0 tries 0.0 upTo
    print ("k = 2, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k2)
    k3 <- avgMillerRabin 3 0 tries 0.0 upTo
    print ("k = 3, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k3)
    k5 <- avgMillerRabin 5 0 tries 0.0 upTo
    print ("k = 5, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k5)
    k10 <- avgMillerRabin 10 0 tries 0.0 upTo
    print ("k = 10, first " ++ show upTo ++ " carMichael numbers, average of " ++ show tries ++ " tests. Percentage of Fermat pseudoprimes : " ++ show k10)

{------------------------------------------------------------------------------

Assignment 6_2

Hours spent: 1h

------------------------------------------------------------------------------}

-- Source: https://oeis.org/A000043
mersenneExp :: [Integer]
mersenneExp = [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279, 2203, 2281, 3217, 
                4253, 4423, 9689, 9941, 11213, 19937, 21701, 23209, 44497, 86243, 110503, 132049, 
                216091, 756839, 859433, 1257787, 1398269, 2976221, 3021377, 6972593, 13466917, 
                20996011, 24036583, 25964951, 30402457, 32582657, 37156667, 42643801, 43112609]

-- discoverMersenneN startingPrime -> # of primeMR tests -> primes list -> max # of mersenne primes -> running mersenne primes 
discoverMersenneN :: Int -> Int -> [Integer] -> Int -> [Integer] -> IO [Integer]
discoverMersenneN iPrime t (p:ps) n xs =
    if (n == (length xs))
        then return xs
        else do
            let mp = 2 ^ (primes !! iPrime) - 1
            isPrime <- primeMR t mp
            if isPrime 
                then discoverMersenneN (iPrime+1) t ps n $ xs ++ [p]
                else discoverMersenneN (iPrime+1) t ps n xs

-- startingPrime -> current Iter -> stop Iter -> # of primeMR tests -> primes list -> max # of mersenne primes -> Orderd mersennes -> Only mersennes
avgDiscoverMersenneN :: Int -> Int -> Int -> Int -> [Integer] -> Int -> Int -> Int -> IO (Float, Float)
avgDiscoverMersenneN sPrime b stop t ps upTo os cs =
    if (b == stop)
        then return ((fromIntegral cs) / (fromIntegral stop) * 100, (fromIntegral os) / (fromIntegral stop)* 100)
        else do
            m <- discoverMersenneN sPrime t ps upTo []
            -- check if order of mersenne list is correct from small to big
            let osi = fromEnum (m == take upTo mersenneExp)
            -- check if all mersenne primes are correct
            let csi = fromEnum (m == (intersect m mersenneExp))
            avgDiscoverMersenneN sPrime (b+1) stop t ps upTo (os + osi) (cs + csi)

-- Find the first 15 mersenne exponents
ass6b = do
    let upTo = 15
    let bench = 10
    -- primeMR 1 mp
    m1 <- avgDiscoverMersenneN 0 0 bench 1 primes upTo 0 0 
    print ("Avg of " ++ show bench ++ " runs with: primeMR 1 x and " ++ show upTo ++ " mersenne primes: " ++ (show (fst m1)) ++ "% of the lists contained only correct values and " ++ (show (snd m1)) ++ "% orderd by small to large")
    -- primeMR 10 mp
    m10 <- avgDiscoverMersenneN 0 0 bench 10 primes upTo 0 0 
    print ("Avg of " ++ show bench ++ " runs with: primeMR 10 x and " ++ show upTo ++ " mersenne primes: " ++ (show (fst m10)) ++ "% of the lists contained only correct values and " ++ (show (snd m10)) ++ "% orderd by small to large")


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
    -- bit length check?
    --
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
    -- 
    -- perhaps add hash/seriliaztion to bypass Integer limitation
    let msg = 123456789
    print ("Original message: " ++ show msg)
    let msgEncode = rsaEncode publicKey msg
    print ("Encoded message: " ++ show msgEncode)
    let msgDecode = rsaDecode privateKey msgEncode
    print ("Decoded message: " ++ show msgDecode)