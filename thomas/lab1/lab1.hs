
module Lab1 where
import Data.List
import Data.Char
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
reversablePrime :: Integer -> [Integer]
reversablePrime x = [x | prime (reversal x)]

filterReversablePrimes :: [Integer] -> [Integer] -> [Integer]
filterReversablePrimes (x:xs) res = if x < 10000 
                                    then filterReversablePrimes xs (res ++ (reversablePrime x))
                                    else res

generateReversablePrimes :: [Integer]
generateReversablePrimes = filterReversablePrimes primes []

-- Lab 1.5
getSublistPrimes :: Int -> Integer
getSublistPrimes n = sum (drop n (take (101+n) primes))

findConsecPrime :: Int -> Integer
findConsecPrime n = if prime x then x else findConsecPrime (n+1) where x = getSublistPrimes n

findConsecutivePrime :: Integer
findConsecutivePrime = findConsecPrime 1

-- Lab 1.6
getCounterExamples :: Int -> Int -> [Integer] -> [Integer]
getCounterExamples 0 n res = res
getCounterExamples num n res = if prime x 
                               then getCounterExamples num (n+1) res 
                               else getCounterExamples (num-1) (n+1) (res ++ [x])
                               where x = (product (take n primes)) + 1

findCounterExamples :: Int -> [Integer]
findCounterExamples n = getCounterExamples n 0 []

-- Lab 1.7
doDouble :: Char -> Char
doDouble num = if x >= 10 
               then intToDigit (x-9) 
               else intToDigit x where x = digitToInt num * 2

decideDouble :: Int -> Char -> Char
decideDouble n num = if mod n 2 == 0 
                     then doDouble num 
                     else num

calculateSumDigits :: Int -> [Char] -> [Char] -> [Char]
calculateSumDigits n [] res = res
calculateSumDigits n (x:xs) res = calculateSumDigits (n+1) xs (res ++ [decideDouble n x])

luhnMiddle :: [Char] -> [Int]
luhnMiddle number = map digitToInt x where x = calculateSumDigits 1 (reverse number) []

luhnAlgorithm :: [Char] -> Bool
luhnAlgorithm number = mod (sum (luhnMiddle number) * 9) 10 == 0

luhn :: Integer -> Bool
luhn number = luhnAlgorithm (show number)

masterCheckOne :: Integer -> Bool
masterCheckOne number = x >= 2221 && x <= 2720 where x = div number 1000000000000

masterCheckTwo :: Integer -> Bool
masterCheckTwo number = x >= 51 && x <= 55 where x = div number 100000000000000
                    
isMaster :: Integer -> Bool
isMaster number = luhn number && (masterCheckOne number || masterCheckTwo number)

visaCheck :: Integer -> Bool
visaCheck number = x == "4" where x = take 1 (show number)

isVisa :: Integer -> Bool
isVisa number = luhn number && visaCheck number

aeCheck :: Integer -> Bool
aeCheck number = x == "34" || x == "37" where x = take 2 (show number)

isAmericanExpress :: Integer -> Bool
isAmericanExpress number = luhn number && aeCheck number