
module Lab1 where
import Data.List
import Test.QuickCheck

{-------------------------------------------------------
    Given code and modified functions
--------------------------------------------------------}
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

-- Generate prime lists incrementally
-- Note: assumes that Integer n is a prime
primes' :: Integer -> [Integer]
primes' n = n : filter prime [(n+1)..]

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

divide :: Integer -> Integer -> Bool
divide n m = rem m n == 0

isPrime :: Integer -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

{-------------------------------------------------------
    Assignment 1 part 1
    Time: 45 min
    Most of it was spend fighting with quickcheck
--------------------------------------------------------}

-- 1^2 + 2^2 + .. + n^2
lsidea :: Int -> Int
lsidea n = sum [x ^ 2 | x <- [1..n]]

-- n(n + 1)(2n +1) / 6
rsidea :: Int -> Int
rsidea n = div (n * (n + 1) * (2 * n + 1)) 6

-- quickcheck
checkWorkshop2 = quickCheck (\ (Positive x) -> lsidea x == rsidea x)

{-------------------------------------------------------
    Assignment 1 part 2
--------------------------------------------------------}

-- 1^3 + 2^3 + .. + n^3
lsideb :: Int -> Int
lsideb n = sum [x ^ 3 | x <- [1..n]]

-- (n(n+1) / 2)^2
rsideb :: Int -> Int
rsideb n = div  (n * (n + 1)) 2 ^ 2

checkWorkshop3 = quickCheck (\ (Positive x) -> lsideb x == rsideb x)

{-------------------------------------------------------
    Assignment 2
    Time: 20 min

    Checking is slow the set grows exponential.

    Note that we only check for the length of the 2 lists.
    We do not check if the lists contain the identical sequences.
--------------------------------------------------------}

-- |A| = n
setA :: Int -> Int
setA i = length (subsequences [1..i])

-- |>A| = 2^n
subSets :: Int -> Int
subSets i = 2 ^ i


checkWorkshop4 = quickCheck (\ (Positive x) -> setA x == subSets x)

{-------------------------------------------------------
    Assignment 3
    Time: 30 min

    Difficult to test correctly because of exponential n growth.

    Again we do not verify the contents of the lists, only the length.
--------------------------------------------------------}

numberPerm, numberObjects :: Int -> Int
numberPerm n = length (permutations [1..n])
numberObjects n = product [1..n]

checkWorkshop5 = quickCheckResult (\ (Positive x) -> (x < 10) --> numberPerm x == numberObjects x)

{-------------------------------------------------------
    Assignment 4
    Time: 20 min

    I compare my results with the intersection of 2 prime x result,
    the normal version and the reverse version.
    I dont think that quickcheck can help because this assignment
    does not contain random variables, the values are fixed.
--------------------------------------------------------}

-- Generate the solution
findReversePrime = [x | x <- [1..10001], isPrime x && isPrime (reversal x)]

-- Generate the individual prime numbers
checkPrim = [x | x <- [1..10001], isPrime x]
checkPrimeRev = [x | x <- [1..10001], isPrime (reversal x)]

-- Compare the solution with the individual results
check4 = quickCheck (findReversePrime == intersect checkPrim checkPrimeRev)

{-------------------------------------------------------
    Assignment 5
    time: 30 Minutes

    Biggest challenge was making the prime generation nice.
--------------------------------------------------------}
check5 = consecPrime (take 101 primes)

-- Iterate through the prime lists incrementally
incrPrimes :: [Integer] -> [Integer]
incrPrimes xs = tail xs ++ filter prime[(last xs +1)..]

-- Find the smallest result
consecPrime ::[Integer] -> Integer
consecPrime xs
  | isPrime(sum xs) = sum xs
  | otherwise = checkFurther
  where checkFurther = consecPrime (take 101 (incrPrimes xs))

{-------------------------------------------------------
    Assignment 6
    time: 40 minutes

    ugly hardcoded Int , has to go
    wanted quickcheck solution, didnt find one...
    Not sure how quickcheck can be used to find the smallest counterexample?
    Perhaps with a fixed range starting from 1?
--------------------------------------------------------}

-- find counterexample, 
assignment6 :: Int -> [Integer]
assignment6 n 
  | not(isPrime((product(take n primes)) + 1)) =  take n primes
  | otherwise = checkFurther
  where checkFurther = assignment6 (n+1)

{-------------------------------------------------------
    Assignment 7
    time: 70 minutes

    source digg: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
    
    Missing: creditcard testcases.
--------------------------------------------------------}

testacc = 79927398713

-- Parse integer into list 
-- Ex: 123 -> [1,2,3]
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

luhn :: [Integer] -> Bool
luhn n
  | last n == mod (9 * (sum (init[if x < 10 then x else (x-9) | 
    x <- reverse(zipWith (*) (reverse n) (cycle [1,2]))]))) 10 = True
  | otherwise = False


assignment7 = luhn(digs(testacc))
-- Unimplemented
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = False

isMaster :: Integer -> Bool
isMaster n = False
  
isVisa :: Integer -> Bool
isVisa n = False

{-------------------------------------------------------
    Assignment 8
    time: 90 minutes

    Problem was understanding the problem correctly
--------------------------------------------------------}

accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x /= Carl) && (x /= Matthew)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not(accuses Matthew x) && not(accuses Peter x)
accuses Arnold x = accuses Matthew x /= accuses Peter x
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers x = [n | n <- boys, accuses n x]

-- 3 boys are telling the truth, they are accusing the guilty boy.
guilty, honest :: [Boy]
guilty = [n | n <- boys, length (accusers n) == 3]
honest = accusers (head guilty)
