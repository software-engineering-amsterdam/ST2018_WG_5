
module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

primes' :: Integer -> [Integer]
primes' n = n : filter prime [(n+1)..] -- CHECKEN i assume n = prime!

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

-- test examples

sentence = "Sentences can go " ++ onAndOn
onAndOn = "on and on " ++ onAndOn

sentences = "Sentences can go on":
  map (++ " and on") sentences

myall :: (a -> Bool) -> [a] -> Bool
myall p [] = True
myall p (x:xs) = p x && myall p xs

list2p :: Eq a => [a] -> a -> Bool
list2p = flip elem

myallTest :: [Int] -> [Int] -> Bool
myallTest = \ ys xs -> let p = list2p ys in 
  all p xs == myall p xs

divide :: Integer -> Integer -> Bool
divide n m = rem m n == 0

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ d -> not(divide d n))[2..n-1]

isPrime' :: Integer -> Bool
isPrime' n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

-- 
--
-- 1
--
-- exercise 2 from the workshop
--

lside1 :: Int -> Int
lside1 n = sum [x ^ 2 | x <- [1..n]]

rside1 :: Int -> Int
rside1 n = div (n * (n + 1) * (2 * n + 1)) 6

-- failes with -2 negative numbers
-- true because 1,2,...
-- https://stackoverflow.com/questions/39291494/only-generate-positive-integers-with-quickcheck
checkq12 = quickCheck (\ (Positive x) -> lside1 x == rside1 x)

--
-- exercise 3 from the workshop
--

lside2 :: Int -> Int
lside2 n = sum [x ^ 3 | x <- [1..n]]

rside2 :: Int -> Int
rside2 n = (div  (n * (n + 1)) 2) ^ 2

checkq22 = quickCheck (\ (Positive x) -> lside2 x == rside2 x)

--
-- 2
--
-- set a = n dan gek ding a = 2^n

left2 :: [a] -> Int
left2 i = length (subsequences i)

right2 :: [a] -> Int
right2 i = 2 ^ (length i)

-- quichcheck, problem is grote nummers?

--
-- 3
--
-- Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n].
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- copy pasta
-- https://stackoverflow.com/questions/40097116/get-all-permutations-of-a-list-in-haskell
permutate' :: (Eq a) => [a] -> [[a]]
permutate' [] = [[]]
permutate' l = [a:x | a <- l, x <- (permutate' $ filter (\x -> x /= a) l)]

-- bs is checken if list is same? perhaps factorial?

--
-- 4
-- 
--reversal :: Integer -> Integer
--reversal = read . reverse . show
--testp :: [Ord] => a -> [a]
q4 = [x | x <- [1..10001], isPrime' x && isPrime' (reversal x)]

checkprime = [x | x <- [1..10001], isPrime' x]
checkprimerev = [x | x <- [1..10001], isPrime' (reversal x)]

check4 = quickCheck (q4 == (intersect checkprime checkprimerev))

--
-- 5
--
q5 = take 5 primes

primes2 :: [Integer] -> [Integer]
--primes2 (x:xs) = xs ++ filter prime[(last(xs)+1)..]
primes2 xs = tail(xs) ++ filter prime[(last(xs)+1)..]

testq5 ::[Integer] -> Integer
--testq5 xs = isPrime'(sum xs)
--testq5 p (x:xs) = p x && myall p xs
testq5 xs
  | isPrime'(sum xs) = sum xs
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
  | not(isPrime'((product(take n primes)) + 1)) =  take n primes
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
accuses :: Boy -> Boy -> Bool
-- case Matthew
accuses Matthew x = not(x == Carl) && not(x == Matthew)
-- case Peter
accuses Peter x = (x == Matthew) || (x == Jack)
-- case Jack
accuses Jack x = not(accuses Matthew x) && not(accuses Peter x)
-- case Arnold
accuses Arnold x = (accuses Matthew x) /= (accuses Peter x)
-- case Carl
accuses Carl x = not (accuses Arnold x)
-- otherwise
-- accuses x y = False

accusers :: Boy -> [Boy]
accusers x = [n | n <- boys, accuses n x]

guilty, honest :: [Boy]
guilty = [n | n <- boys, (length(accusers n) == 3)]
honest = accusers (guilty!!0)
