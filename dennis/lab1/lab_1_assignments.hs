import Test.QuickCheck
import Data.List

-- 1.2 - 30 min
--mySum :: Int -> Int
--mySum 0 = 0
--mySum x = x + mySum (x-1)
--quickCheck (\ (Positive i) -> mySum i == (i * (i+1)) `div` 2)

-- 1.3 - 15 min
--myPowerTwoSum :: Int -> Int
--myPowerTwoSum 0 = 0
--myPowerTwoSum x = (x^2) + myPowerTwoSum (x-1)
--quickCheck (\ (Positive i) -> myPowerTwoSum i == ((i * (i+1)) * (2*i+1)) `div` 6)

-- 2 - 30 min
--getLengthSubsequences :: Int -> Int
--getLengthSubsequences n = (length (subsequences [1 .. n]))
--quickCheckResult (\ (Positive n) -> getLengthSubsequences n == (2^n))
-- This is hard to test due to the runtime of generating subsequences for long lists.
-- We are actually testing the specification of subsequences.

-- 3 - 15 min
--fac :: Int -> Int
--fac n = if n == 0 then 1 else n * fac (n-1)
--getLengthPermutations :: Int -> Int
--getLengthPermutations n = (length (permutations [1 .. n]))
-- This is hard to test due to the runtime of generating permutations for long lists.
-- We are actually testing the specification of permutations.

-- 4 - 15 min
--isPrime :: Int -> Bool
--isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]
--reversal :: Int -> Int
--reversal = read . reverse . show
--findReversalPrimes :: Int -> [Int]
--findReversalPrimes 0 = []
--findReversalPrimes n = if isPrime n && isPrime (reversal n) then n:findReversalPrimes (n-1) else findReversalPrimes (n-1)
-- I would test this function by using QuickCheck and checking whether the number is a prime and the reverse of the number is a prime.

-- 5 - 30 min
--isPrime :: Integer -> Bool
--isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]
--primes :: [Integer]
--primes = 2: 3: sieve (tail primes) [5,7..]
-- where
--  sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
--                  where (h,~(_:t)) = span (< p*p) xs
--findSmallestSumConsecutivePrimes :: Int -> Int -> Integer
--findSmallestSumConsecutivePrimes 0 0 = 0
--findSmallestSumConsecutivePrimes s n = if isPrime (sum (drop s (take n primes))) then sum (drop s (take n primes)) else findSmallestSumConsecutivePrimes (s+1) (n+1)
--findSmallestSumConsecutivePrimes 0 1
-- It might be possible to test the prime generator by checking if every number is a prime.

-- 6 - 45 min
--primes :: [Integer]
--primes = 2: 3: sieve (tail primes) [5,7..]
-- where
--  sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
--                  where (h,~(_:t)) = span (< p*p) xs
--isPrime :: Integer -> Bool
--isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]
--listProduct :: [Integer] -> Integer
--listProduct xs = foldl (*) 1 xs
--findSmallestCounterExample :: Int -> [Integer]
--findSmallestCounterExample n = if isPrime ((listProduct (take n primes))+1) then findSmallestCounterExample (n+1) else take n primes
--findSmallestCounterExample 1

-- 7 - 90 min
--luhnDouble :: Int -> Int
--luhnDouble x = if (x*2) > 10 then (x*2) - 9 else x*2
--doubleOddDigitsFromRight :: Int -> [Int] -> [Int] -> [Int]
--doubleOddDigitsFromRight n [] r = r
--doubleOddDigitsFromRight n (x:xs) r = if (n `mod` 2) == 0 then doubleOddDigitsFromRight (n+1) xs (r ++ [(luhnDouble x)]) else doubleOddDigitsFromRight (n+1) xs (r ++ [x])
--digits :: Integer -> [Int]
--digits = map (read . (:[])) . show
--luhn :: Integer -> Bool
--luhn x = ((sum (doubleOddDigitsFromRight 1 (digits x) []) * 9) `mod` 10) == 0
--numDigits :: Integer -> Int
--numDigits n = length (digits n)
--isAmericanExpress :: Integer -> Bool
--isAmericanExpress x = (take 2 (digits x) == [3,4] || take 2 (digits x) == [3,7])  && luhn x && numDigits x == 15
--isVisa :: Integer -> Bool
--isVisa x = take 1 (digits x) == [4] && luhn x && ((numDigits x == 13) || (numDigits x == 16))
--isMaster :: Integer -> Bool
--isMaster x = (take 2 (digits x) == [5,1] || take 2 (digits x) == [5,5])  && luhn x && numDigits x == 16
--isAmericanExpress 371449635398431
-- This program is manually tested with valid and non-valid numbers.

-- 8 - 60 min
--data Boy = Matthew | Peter | Jack | Arnold | Carl
--           deriving (Eq,Show)
--boys = [Matthew, Peter, Jack, Arnold, Carl]
-- This assignment was done with the group.


