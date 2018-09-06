import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Data.List
import Numeric.Natural

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

sumOfNSquared :: Int -> Int
sumOfNSquared n = sum [x * x | x <- [1..n]]

-- 2 hours
-- Assignment 1 part 1
assignment1first :: Int -> Int
assignment1first n = div (n * (n + 1) * ((2 * n) + 1)) 6

assignment1FirstTest = quickCheckResult (\ (Positive x) -> sumOfNSquared x == assignment1first x)

-- 0.5 hours
-- Assignment 1 part 2
sumOfNToThePowerOf :: Int -> Int -> Int
sumOfNToThePowerOf n power = sum [x^power | x <- [1..n]]

assignment1second :: Int -> Int
assignment1second n = (div (n * (n+1)) 2) ^ 2

assignment1SecondTest = quickCheckResult (\ (Positive x) -> sumOfNToThePowerOf x 3 == assignment1second x)

-- 1 hour
-- Assignment 2
positivesList :: Int -> [Int]
positivesList x = [1..x]
assignment2Test = quickCheckResult (\ (Positive x) -> (2^x) == length (subsequences (positivesList x)) )
-- It is hard to test, since there is a very big increase in things to compute

-- Checking if 2 to the power of x is the same as cardinal of the power set of A
-- Of course it is needed that the subsequences functions works as intended

-- 0.5 hours
-- Assignment 3
factorial n = if n < 2 then 1 else n * factorial (n-1)
assignment3Test = quickCheckResult (\ (Positive x) -> (factorial x) == length (perms (positivesList x)) )
-- It is hard to test, since there is a very big increase in things to compute

-- Checking if the factorial of x is the same as the formula in the assignment
-- Of course it is needed that the perms functions works as intended

-- 1 hour
-- Assignment 4
reversal :: Int -> Int
reversal  = read . reverse . show

primes :: [Int]
primes = 2 : filter prime [3..]

primesFrom :: Int -> [Int]
primesFrom x = filter prime [x..]

takePrimesFrom :: Int -> Int -> [Int]
takePrimesFrom amount from = take amount (primesFrom from)

prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primesSmallerThen :: Int -> [Int]
primesSmallerThen n = takeWhile (\x -> x < n) primes

assignment4Filter :: [Int]
assignment4Filter = filter (\x -> prime (reversal x)) (primesSmallerThen 1000)
-- Q: How would you test this function, by the way?
-- A: probably by checking random (or all) numbers on being a prime after reversing

-- 0.75 hours
-- Assignment 5
-- total = 37447, range = (83 - 677)
listOfhundredOnes :: [[Int]]
listOfhundredOnes = [takePrimesFrom 101 x | x <- [1..]]
assignment5 = sum (filter (\xs -> prime(sum xs)) listOfhundredOnes !! 1)

-- 1 hour
-- Assignment 6
-- Smallest counter example is 30031
takePrimes :: Int -> [Int]
takePrimes x = take x primes
assignment6 = (filter (\x -> not (prime(x))) [(product (takePrimes x) + 1) | x <- [1..]]) !! 0

-- 2 hours
-- Assignment 7
-- performs the double operation on a digit
luhnDouble digit = if doubled > 9 then doubled - 9 else doubled
  where doubled = digit * 2
-- sums a list of digits after performing the luhn double
sumLuhnDoubles digits = sum (map (\x -> luhnDouble x) digits)

-- converts an integer to an array of digits out of which the integer consisted
digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- gets every 2nd, 4th, 6th etc.. item of a list
everySecond (x:y:xs) = y : everySecond xs;
everySecond _ = []

-- gets the 1st, 3rd, 5th etc.. items of a list
getOtherDigits (x:y:xs) = x : getOtherDigits xs;
getOtherDigits [x] = [x]
getOtherDigits _ = []

-- checks if the card number is valid by the luhn algorithm
luhn :: Int -> Bool
luhn card = ((sum ((map (\x -> luhnDouble x) secondDigits) ++ otherDigits)) + checkDigit) `mod` 10 == 0
    where
      secondDigits = everySecond (reverse digitsList) -- array of all second digits in the number
      otherDigits = tail (getOtherDigits (reverse (digitsList))) -- array of all non second digits in the number
      digitsList = digits card -- the digits of the card to check as array
      checkDigit = last digitsList -- the last check digit

isAmericanExpress :: Int -> Bool
isAmericanExpress card = (((take 2 digitsList) == [3,4]) ||
                          ((take 2 digitsList) == [3,7]) ) &&
                          (length digitsList) == 15 &&
                          luhn card
  where digitsList = digits card

isMaster :: Int -> Bool
isMaster card = (((take 4 digitsList) == [2,2,2,1]) ||
                ((take 4 digitsList) == [2,7,2,0]) ||
                ((take 2 digitsList) == [5,1]) ||
                ((take 2 digitsList) == [5,5])) &&
                (length digitsList) == 16 &&
                luhn card
  where
    digitsList = digits card

isVisa :: Int -> Bool
isVisa card = (digitsList !! 0) == 4 && (length digitsList) == 16 && luhn card
  where
    digitsList = digits card

-- This is best testable by testing various valid/invalid numbers for each type of card
testVisa = [(isVisa 4111111111111111), (isVisa 4012888888881881), not (isVisa 4111132111111111), not (isVisa 4012588888881881)]
testMaster = [(isMaster 5555555555554444), (isMaster 5105105105105100), not (isMaster 5555555553554444), not (isMaster 5105125105105100)]
testAmerican = [(isAmericanExpress 378282246310005), (isAmericanExpress 371449635398431), (isAmericanExpress 378734493671000),
                not (isAmericanExpress 378282245310005), not (isAmericanExpress 371449235398431), not (isAmericanExpress 378734494671000)]

-- 1.5 hours
-- Assignment 8
-- contains the different boys present in the story
data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

-- list of  all possible boys in the story
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- checks if a boy accuses the other Boy
accuses :: Boy -> Boy -> Bool
accuses Matthew x = (not (x == Carl)) && (not (x == Matthew))
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = (accuses Matthew x) /= (accuses Peter x)
accuses Carl x = not (accuses Arnold x)

-- accusers of the provided Boy
accusers :: Boy -> [Boy]
accusers x = [y |y <- boys, accuses y x]

-- list of guilty boys in the story
-- these are all boys who are accused by 3 others
guilty :: [Boy]
guilty = [x | x <- boys, length (accusers x) == 3]

-- list of honest boys (who correctly pointed out the culprit)
honest :: [Boy]
honest = getHonestPerson guilty []

getHonestPerson :: [Boy] -> [Boy] -> [Boy]
getHonestPerson [] res = res
getHonestPerson (y:ys) res = getHonestPerson ys (filter (\x -> accuses x y) boys ++ res)
