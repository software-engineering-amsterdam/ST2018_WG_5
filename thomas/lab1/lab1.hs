
module Lab1 where
import Data.List
import Debug.Trace
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Lab 1.1
-- Workshop 2
-- 40 minutes
sumSquares :: Int -> Int
sumSquares 1 = 1
sumSquares n = n^2 + sumSquares (n-1)

fastSumSquares :: Int -> Int
fastSumSquares n = div (n * (n + 1) * ((2 * n) + 1)) 6

checkWorkshopTwo = quickCheckResult (\ (Positive n) -> sumSquares n == fastSumSquares n)

-- Workshop3
-- 5 minutes
sumTripleSquares :: Int -> Int
sumTripleSquares 1 = 1
sumTripleSquares n = n^3 + sumTripleSquares (n-1)

fastSumTripleSquares :: Int -> Int
fastSumTripleSquares n = (div (n*(n+1)) 2)^2

checkWorkshopThree = quickCheckResult (\ (Positive n) -> sumTripleSquares n == fastSumTripleSquares n)

-- Lab 1.2
-- Workshop 4
testList :: [Int] -> Bool
testList a = 2 ^ (length a) == length (subsequences a)

testGenList :: Int -> Bool
testGenList n = 2 ^ n == length (subsequences [1..n])

checkWorkshopFour = quickCheckResult (\ (Positive a) -> testGenList a)

-- Lab 1.3
-- Workshop 5
permLength :: [Int] -> Int
permLength x = product [1..(length x)] 

checkWorkshopFive = quickCheckResult (\ a -> length (permutations a) == permLength a)

-- Lab 1.4
generateReversablePrimes :: () -> [Int]
-- generateReversablePrimes = 