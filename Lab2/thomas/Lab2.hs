module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{------------------------------------------------------------------------------

  Assignment 1
  Your programmer Red Curry...

  Hours spent: 1
  Answers:
  - Based on the generated example outputs one could see that all quartiles are
    around 2500 examples big. So it could said that the generated float are 
    random

  ------------------------------------------------------------------------------}

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
                p <- getStdRandom random
                ps <- probs (n-1) 
                return (p:ps)
    
-- Count the quartile a float is in and if it is in that specific quartile
-- then add the number of that quartile to the result list
countFloatQuartiles :: [Float] -> [Int] -> [Int]
countFloatQuartiles [] res = res
countFloatQuartiles (y:ys) res 
    | y >= 0.0 && y < 0.25 = countFloatQuartiles ys (1:res)
    | y >= 0.25 && y < 0.5 = countFloatQuartiles ys (2:res)
    | y >= 0.5 && y < 0.75 = countFloatQuartiles ys (3:res)
    | y >= 0.75 && y < 1 = countFloatQuartiles ys (4:res)

-- Count the number of times the given integer n occurs in a list
countInt :: Int -> [Int] -> Int -> Int
countInt n [] r = r
countInt n (y:ys) r = if n == y 
                      then countInt n ys (r+1)
                      else countInt n ys r

-- Count the number of times a float in each quartile happens
startCountingQuartiles :: Int -> IO [Int]
startCountingQuartiles n = do 
                            xs <- (probs n)
                            let xr = countFloatQuartiles xs []
                            let x1 = countInt 1 xr 0
                            let x2 = countInt 2 xr 0
                            let x3 = countInt 3 xr 0
                            let x4 = countInt 4 xr 0
                            return [x1, x2, x3, x4]

-- startCountingQuartiles 10000 gives the following outputs
-- [2471,2441,2566,2522]
-- [2395,2569,2499,2537]
-- [2530,2496,2508,2466]
-- [2454,2476,2509,2561]

{------------------------------------------------------------------------------

  Assignment 2
  Recognizing triangles

  Hours spent: 1
  Answers:
  - If you want to test if the triangles are correct you could use logic to generate
    specific types of triangles, but in order to do this more or less the same logic
    would be used for generating the specific triangles as the logic is used to 
    determine the type of triangle. So this would not be very effective.
  - So I choose to pick some examples of which I know which kind of traingles they are
    and make sure the algorithm determines the right ones.

  ------------------------------------------------------------------------------}
data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- If the combination of lengths is impossible than it is not a triangle
isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = (a + b) <= c ||
                     (c + a) <= b ||
                     (b + c) <= a

-- Check if the length of all legs are the same, if this is  
-- the case then it is an equilateral triangle
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && b == c

-- Using the Pythagorean theorem it can be determined if a triangle is
-- rectagnular triangle
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = (a^2 + b^2) == c^2 || 
                      (b^2 + c^2) == a^2 || 
                      (c^2 + a^2) == b^2

-- If two legs of a triangle are the same then it is an Isosceles
-- triangle
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = a == b || b == c || c == a

-- The order of the guards is important for this assignment so
-- it should not be changed
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | isNoTriangle a b c = NoTriangle
    | isEquilateral a b c = Equilateral
    | isRectangular a b c = Rectangular
    | isIsosceles a b c = Isosceles
    | otherwise = Other

testNoTriangle = all (== NoTriangle) [triangle 3 4 7, triangle 9 81 3, triangle 12 2 45, triangle 10 4 1]
testEquilateral = all (== Equilateral) [triangle 6 6 6, triangle 4 4 4, triangle 80 80 80, triangle 1 1 1]
testRectangular = all (== Rectangular) [triangle 3 4 5, triangle 5 12 13, triangle 8 15 17, triangle 7 24 25]
testIsosceles = all (== Isosceles) [triangle 1 2 2, triangle 3 3 4, triangle 3 3 5, triangle 9 9 7]
testOther = all (== Other) [triangle 2 3 4, triangle 4 5 6, triangle 7 8 9, triangle 9 11 10]
assignment2Test = testNoTriangle && testEquilateral && testRectangular && testIsosceles && testOther
-- Output: assignment2Test
-- True

{------------------------------------------------------------------------------

  Assignment 3
  Testing properties strength

  Hours spent: 1
  Answers:
  a) All properties were implemented
  b) The result of assignment3Sorted in descending order of strength is:
     property 1, property 3, property 4, property 2
     But the output could also have been:
     property 1, property 4, property 3, property 2
     Because property 3 and 4 have the same strength

  ------------------------------------------------------------------------------}
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Given function which determine if a property is stronger or weaker
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

-- Implemented the bigger than three property
biggerThanThree :: Int -> Bool
biggerThanThree x = x > 3

-- Retrieved the domain of 2.3
integerDomainAssignment3 = [-10..10]
domainAssignment3 :: [Int]
domainAssignment3 = [fromInteger x | x <- integerDomainAssignment3]

-- Implemented the four properties
prop1 :: Int -> Bool
prop1 x = even x && biggerThanThree x
prop2 :: Int -> Bool
prop2 x = even x || biggerThanThree x
prop3 :: Int -> Bool
prop3 x = (even x && biggerThanThree x) || even x
prop4 :: Int -> Bool
prop4 x = (even x && biggerThanThree x) || even x
ass3All = [(prop1, "prop1"), (prop2, "prop2"), (prop3, "prop3"), (prop4, "prop4")]

propertySorting :: (Int -> Bool, String) -> (Int -> Bool, String) -> Ordering  
propertySorting (x, nameX) (y, nameY)
    | stronger domainAssignment3 y x && stronger domainAssignment3 x y = EQ
    | stronger domainAssignment3 y x = GT
    | otherwise = LT

assignment3Sorted = map snd (sortBy propertySorting ass3All)
-- Output: assignment3Sorted
-- ["prop1","prop3","prop4","prop2"]

{------------------------------------------------------------------------------

  Assignment 4
  Recognizing Permutations

  Hours spent: 2
  Answers:
  - The following two properties have to hold: The lists should have the same length
    and every element of list a has to also be part of list b.
  - Based on the following output there are equally as strong:
    
    stronger shouldBeTrues splitSameLength splitSameElements
    True

    stronger shouldBeTrues splitSameElements splitSameLength
    True
  - The test could be automated by comparing the isPermutation function with the
    efficientIsPermutation function for random lists. But the isPermutation 
    function becomes very slow for large lists. So predetermined examples were used

  ------------------------------------------------------------------------------}
-- If a is an element of all permutations of b then it is a permutation of b
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = a `elem` (permutations b)

efficientIsPermutation :: Eq a => [a] -> [a] -> Bool
efficientIsPermutation a b = sameLength a b && sameElements a b

sameLength :: [a] -> [a] -> Bool
sameLength xs ys = length xs == length ys

-- Check if all elements of xs exist in ys
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = all (\ x -> x `elem` xs) ys

-- Split so the stronger function could be used on the test lists
splitSameElements :: Eq a => ([a], [a]) -> Bool
splitSameElements (a,b) = sameElements a b 
-- Split so the stronger function could be used on the test lists
splitSameLength :: Eq a => ([a], [a]) -> Bool
splitSameLength (a,b) = sameLength a b

shouldBeTruesAss4 = [([1,2,3], [3,2,1]), ([2,3,1], [1,2,3])]
shouldBeFalsesAss4 = [([1,2,3], [1,2,4]), ([1,2,3], [1,2,3,4])]

testTrues = all (== True) (map (\ (a, b) -> efficientIsPermutation a b) shouldBeTruesAss4)
wronglyTestFalses = all (== True) (map (\ (a, b) -> efficientIsPermutation a b) shouldBeFalsesAss4)
testFalses = all (== False) (map (\ (a, b) -> efficientIsPermutation a b) shouldBeFalsesAss4)
wronglyTestTrues = all (== False) (map (\ (a, b) -> efficientIsPermutation a b) shouldBeTruesAss4)
-- Outputs in order:
-- True
-- False
-- True
-- False
-- These are the correct outputs

{------------------------------------------------------------------------------

  Assignment 5
  Recognizing and generating derangements

  Hours spent: 1.5
  Answers:

  - Derangments are permutations for which no element can stay on the same position
  - Properties in desc order of strength:
    - isDerangement
    - sameLength / sameElements
  - This process can not be automated without using more or less the same logic twice
    to determine or generate examples of derangments and non derangments
  ------------------------------------------------------------------------------}

-- This function checks if no number finds itself on the same position, this function
-- makes the assumption that no doubles are used
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) [] = False
isDerangement [] (y:ys) = False
isDerangement (x:xs) (y:ys) = (x /= y) && isDerangement xs ys 

-- This function filters out the not derangments from the permutations list which
-- results in list with only derangments
deran :: Eq a => [a] -> [[a]]
deran lst = filter (isDerangement lst) (permutations lst)

-- Split so the stronger function could be used on the test lists
splitIsDerangement :: Eq a => ([a], [a]) -> Bool
splitIsDerangement (a,b) = isDerangement a b

shouldBeTruesAss5 = [([1,2,3], [3,1,2]), ([1,2,3,4], [2,3,4,1])]
shouldBeFalsesAss5 = [([1,2,3], [4,3,2,1]), ([1,2,3], [3,2,1])]

test5Trues = all (== True) (map (\ (a, b) -> isDerangement a b) shouldBeTruesAss5)
wronglyTest5Falses = all (== True) (map (\ (a, b) -> isDerangement a b) shouldBeFalsesAss5)
test5Falses = all (== False) (map (\ (a, b) -> isDerangement a b) shouldBeFalsesAss5)
wronglyTest5Trues = all (== False) (map (\ (a, b) -> isDerangement a b) shouldBeTruesAss5)
-- Outputs in order:
-- True
-- False
-- True
-- False
-- These are the correct outputs

{------------------------------------------------------------------------------

  Assignment 6
  Implementing and testing ROT13 encoding

  Hours spent: 1.5
  Answers:
  - Rot13 shifts every character 13 places to the next character and wraps
    around when the letter 'z' is exceded. So using rot13 twice should give
    the original string

  ------------------------------------------------------------------------------}
-- Calculate the change when 13 is added to integer value of the 
-- character, but use a modulo to wrap the integer value around
-- if it overflows the alphabet
mapChar :: Char -> Char
mapChar c = chr ((((ord c) - (ord 'a') + 13) `mod` 26) + ord 'a')

-- Works the same as the normal mapChar
mapCapitalChar :: Char -> Char
mapCapitalChar c = chr ((((ord c) - (ord 'A') + 13) `mod` 26) + ord 'A')

-- If the given character is in the alphabet then rot13 it
alphabet = ['a' .. 'z']
capitalAlphabet = ['A' .. 'Z']
rotateBy :: [Char] -> [Char] -> [Char]
rotateBy [] res = res
rotateBy (x:xs) res 
    | x `elem` alphabet = rotateBy xs (res++[(mapChar x)])
    | x `elem` capitalAlphabet = rotateBy xs (res++[(mapCapitalChar x)])
    | otherwise = rotateBy xs (res++[x])

rot13 :: [Char] -> [Char]
rot13 str = rotateBy str ""

-- Test if using rot13 two times is the original string 
testReversedRot13 :: [Char] -> Bool
testReversedRot13 str = str == rot13 (rot13 str)

automaticTest6 = quickCheck testReversedRot13
-- Output: +++ OK, passed 100 tests.
{------------------------------------------------------------------------------

  Assignment 7
  Implementing and testing IBAN validation
  In this implementation only dutch IBANs or IBANs with the same length
  are valided succesfully.

  Hours spent: 1.5
  Answers:

  ------------------------------------------------------------------------------}

-- Check if the length of the IBAN is the legitimate length of a dutch IBAN
checkIbanLength :: String -> Bool
checkIbanLength str = length str == 18

-- Move the front character n times to the end of the string
moveToEnd :: String -> Int -> String
moveToEnd (x:xs) 1 = xs++[x]
moveToEnd (x:xs) n = moveToEnd (xs++[x]) (n-1)

-- If a letter is found then replace it with the corresponding number else
-- return the current number
letterReplacer :: Char -> [Char]
letterReplacer c
    | x >= 65 && x <= 90 = show (x - 55) 
    | otherwise = [c]
    where x = ord c

-- Check if the remainder of the number modulo 97 is 1, as a IBAN should be
checkRemainder :: String -> Bool
checkRemainder str = (mod (read (concatMap letterReplacer (moveToEnd str 4))::Integer) 97) == 1

-- Add the remainder check and the dutch length check together
iban :: String -> Bool
iban str = checkIbanLength str && checkRemainder str

-- Valid dutch IBANS and a valid IBAN from Greenland which has the same length
validIbans = ["NL91ABNA0417164300", "GL8964710001000206"]
invalidIbans = ["NL91ABNA04171643001", "NL99BANK0123456789"]

testValidIbans = all (== True) (map iban validIbans)
testInvalidIbans = all (== False) (map iban invalidIbans)
-- Output:
-- True
-- True