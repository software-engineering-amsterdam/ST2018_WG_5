
module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

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

-- 
--
-- 1
--
--
-- Time: 45 min
-- Most of it was spend fighting with quickcheck

-- exercise 2 from the workshop
--

lsidea :: Int -> Int
lsidea n = sum [x ^ 2 | x <- [1..n]]

rsidea :: Int -> Int
rsidea n = div (n * (n + 1) * (2 * n + 1)) 6

-- quickcheck
checkWorkshop2 = quickCheck (\ (Positive x) -> lsidea x == rsidea x)

--
-- exercise 3 from the workshop
--

lsideb :: Int -> Int
lsideb n = sum [x ^ 3 | x <- [1..n]]

rsideb :: Int -> Int
rsideb n = div  (n * (n + 1)) 2 ^ 2

checkWorkshop3 = quickCheck (\ (Positive x) -> lsideb x == rsideb x)

--
-- 2
--
-- Time: 20 min
-- WIP: missing quickstep verification

setA :: [a] -> Int
setA i = length (subsequences i)

subSets :: [a] -> Int
subSets i = 2 ^ length i

--
-- 3
--
-- Time: 30 min
--
-- Note that the proof is dodgy because of the bigly numbers

numberPerm, numberObjects :: Int -> Int
numberPerm n = length (permutations [1..n])
numberObjects n = product [1..n]

checkWorkshop5 = quickCheckResult (\ (Positive x) -> (x < 10) --> numberPerm x == numberObjects x)


--
-- 4
-- 
--reversal :: Integer -> Integer
--reversal = read . reverse . show
--testp :: [Ord] => a -> [a]
q4 = [x | x <- [1..10001], isPrime x && isPrime (reversal x)]

checkprime = [x | x <- [1..10001], isPrime x]
checkprimerev = [x | x <- [1..10001], isPrime (reversal x)]

check4 = quickCheck (q4 == intersect checkprime checkprimerev)

--
-- 5
--
q5 = take 5 primes

primes2 :: [Integer] -> [Integer]
--primes2 (x:xs) = xs ++ filter prime[(last(xs)+1)..]
primes2 xs = tail(xs) ++ filter prime[(last(xs)+1)..]

testq5 ::[Integer] -> Integer
--testq5 xs = isPrime(sum xs)
--testq5 p (x:xs) = p x && myall p xs
testq5 xs
  | isPrime(sum xs) = sum xs
  | otherwise = checkFurther
  where checkFurther = testq5 (take 101 (primes2 xs))

-- hp
-- hoe wat waar ?

--
-- 6 WIP
---
-- ugly hardcoded Int , has to go
-- wanted quickcheck solution, didnt find one...

testq6 :: Int -> [Integer]
testq6 n 
  | not(isPrime((product(take n primes)) + 1)) =  take n primes
  | otherwise = checkFurther
  where checkFurther = testq6 (n+1)

--
-- 7
--

-- copy pasta digs (dig converter)
-- https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
--
-- Should be better (FIX?)

digs :: Integral x => x -> [x]
--digs :: Int -> [Integer]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

testacc = (79927398713)


--luhn :: [Integer] -> Integer--[Integer]
--luhn :: [Integer] -> Integer--[Integer]
luhn :: [Integer] -> Bool
luhn n
  | (last n) == (mod (9 * (sum (init[if x < 10 then x else (x-9) | x <- reverse(zipWith (*) (reverse (n)) (cycle [1,2]))]))) 10) = True
  | otherwise = False

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn(digs(n))

isMaster :: Integer -> Bool
isMaster n = False
  
isVisa :: Integer -> Bool
isVisa n = False
--double2nd n = zipWith (*) n (cycle [1,2])

--
-- 8
--
-- Time: 60 min
-- Problem was understanding the problem correctly

accuses :: Boy -> Boy -> Bool
-- case Matthew
accuses Matthew x = (x /= Carl) && (x /= Matthew)
-- case Peter
accuses Peter x = (x == Matthew) || (x == Jack)
-- case Jack
accuses Jack x = not(accuses Matthew x) && not(accuses Peter x)
-- case Arnold
accuses Arnold x = accuses Matthew x /= accuses Peter x
-- case Carl
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers x = [n | n <- boys, accuses n x]

guilty, honest :: [Boy]
guilty = [n | n <- boys, length (accusers n) == 3]
honest = accusers (head guilty)
