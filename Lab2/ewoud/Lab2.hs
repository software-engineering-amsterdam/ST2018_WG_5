
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Function
import System.Random
import Data.Maybe

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

{------------------------------------------------------------------------------
    Assignment 1
    Time: 90 min

    STATE: Done, but perhaps a better print solution is possible for the answer.
------------------------------------------------------------------------------}

-- Digitize values to a bin range
binValue :: Float -> Int
binValue x 
    | x > 0 && x < 0.25 = 1
    | x >= 0.25 && x < 0.50 = 2
    | x >= 0.50 && x < 0.75 = 3
    | x >= 0.75 && x < 1 = 4
    | otherwise = 0

-- Chi square test
-- See dice example for understanding: https://nl.wikipedia.org/wiki/Chi-kwadraattoets
chiSquare :: Int -> [Int] -> Float
chiSquare n xs = fromIntegral (sum [(x - n)^2 | x <- xs]) / (fromIntegral (length xs) / fromIntegral (length(nub xs)))

-- Count occurences of each value.
-- Source: https://codereview.stackexchange.com/questions/139587/coufromIntegral 
numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

assignment1 :: Int -> IO Float
assignment1 x = do
    -- generate numbers
    genlist <- probs x
    -- bin numbers into quartile
    let binned = [binValue x | x <- (genlist)]
    let bincount = [numTimesFound x binned | x <- [1..4]]
    let pvar = chiSquare x bincount
    -- check null hypothesis: random generator is random!
    -- p value = 0.05, DF = 3, x^2 = 7.815
    if 7.815 > pvar 
        then putStrLn ("not random") 
        else putStrLn ("random!") 
    return pvar

doAssign1 = assignment1 10000

{------------------------------------------------------------------------------
    Assignment 2 
    Recognizing triangles
    Time: 90 min

    Status: Done. Missing is the quickcheck for other cases. Not sure what they
    mean by that...

    Quickcheck defines the triangle solution space and tests our implementation by
    generating random triangles.
------------------------------------------------------------------------------}

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    -- Triangle inequality
    | (a + b < c) || (a + c < b) || (b + c < a) = NoTriangle
    -- An equilateral triangle has all sides the same length
    | a == b && b == c = Equilateral
    -- A rectangled triangle has one of its interior angles measuring 90°.
    | a ^ 2 + b ^ 2 == c ^ 2 || a ^ 2 + c ^ 2 == b ^ 2 || c ^ 2 + b ^ 2 == a ^ 2  = Rectangular
    -- An isosceles triangle has two sides of equal length.
    | a == b || b == c || a == c = Isosceles
    -- Other
    | otherwise = Other

-- Generate triangle testcases
assignment2 = do
    -- Not a triangle!
    quickCheckResult (\ (Positive a) (Positive b) (Positive c) -> triangle a b (a + b + c) == NoTriangle)
    -- Equilateral, same length
    quickCheckResult (\ (Positive a) -> triangle a a a == Equilateral)
    -- Rectangular (90°), using Euclid's formula
    quickCheckResult (\ (Positive m) (Positive n) -> (m > n) --> triangle (m ^ 2 - n ^ 2)(2 * m * n)(m ^ 2 + n ^ 2) == Rectangular)
    -- isocles, check if it is a triangle and generate non equilateral cases
    quickCheckResult (\ (Positive m) (Positive n) -> (n <= m + m && m /= n) --> triangle m m n == Isosceles)
    -- Other ( i dont know what to do about this...)

{------------------------------------------------------------------------------
    Assignment 3 
    Testing properties strength
    Time: x min

    Status: 3b is from michaels version
------------------------------------------------------------------------------}

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker   xs p q = stronger xs q p

-- properties from workshopa 3a
prop1,prop2,prop3,prop4 :: (Int-> Bool, Char)
prop1 = ((\ x -> even x && x > 3), 'a')
prop2 = ((\ x -> even x || x > 3), 'b')
prop3 = ((\ x -> (even x && x > 3) || even x),'c')
prop4 = ((\ x -> (even x && x > 3) || even x), 'd')
ass3All = [prop1, prop2, prop3, prop4]

-- Q 3b

sortProp :: ((Int -> Bool), Char) -> ((Int -> Bool), Char) -> Ordering
sortProp (x, _) (y, _)
  | (stronger [(-10)..10] y x) && (weaker [(-10)..10] y x) = EQ
  | stronger [(-10)..10] y x = GT
  | weaker [(-10)..10] y x = LT

ass3Sorted = map (\x -> snd x) (sortBy sortProp (ass3All))
{------------------------------------------------------------------------------
    Assignment 4 
    Recognizing Permutations
    Time: 45 min
        
    Status: Functional
    Todo: make quickechk pretty (spaghetti logic atm)
    perhaps restructure in a set of requirements, but they seem stupid to me.
    Dont see much value in basic tests like length etc.
------------------------------------------------------------------------------}

-- remove single element from list
popOccurence :: Eq a => a -> [a] -> [a]
popOccurence x [] = []
popOccurence x (y:ys) 
    | x == y = ys
    | otherwise = y : popOccurence x ys

-- check if permutation
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] (y:ys) = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (popOccurence x ys)

-- Check the length of the intersection of 2 unique lists 
-- with both individual lists. 
-- If they are equal we have a permutation because we may assume that there are no duplicates in the list
-- and thus have a 1:1 image domain.
propIsPermutation :: [Integer] -> [Integer] -> Bool
propIsPermutation xs ys = isPermutation (nub xs) (nub ys) == bijective
    where bijective = length (nub xs) == length (intersect (nub xs) (nub ys)) && length (nub ys) == length (intersect (nub xs) (nub ys))

assignment4 = quickCheck propIsPermutation

{------------------------------------------------------------------------------
    Assignment 5 
    Recognizing and generating derangements
    Time: 90 minutes

    State: Done?
    But need to generate better number lists with quickcheck
    Currently 2 issues.
    - Very long number ranges generated by quickcheck?
    - quickechk generates repeating numbers [1,1,1] or [4,4,4]. This should not happen!
------------------------------------------------------------------------------}

-- Derangement according to wikipedia:
-- A derangement is a permutation that is NOT mapped to itself.
propDLength, propDPermutation, propDCommunative:: [Integer] -> [Integer] -> Bool
-- This means that: 
-- A and B == equal length
propDLength xs ys = isDerangement xs ys --> length xs == length ys
-- B = A with B with is F(A) permutation
propDPermutation xs ys = isDerangement xs ys --> isPermutation xs ys
-- B = A then A = B with F(X) derangement (commutative operations)
propDCommunative xs ys = isDerangement xs ys --> isDerangement ys xs
-- propDLength xs ys = isDerangement xs ys ?

isDerangement :: Eq a => [a] -> [a] -> Bool
-- The empty set can be considered a derangement of itself.
isDerangement [] [] = True
isDerangement xs ys
    -- check if equal length
    | length xs /= length ys = False
    -- check if each individual element maps to eachother.
    | or (zipWith (==) xs ys) = False
    | otherwise = True

assignment5 = do
    quickCheckResult propDLength
    quickCheckResult propDPermutation
    quickCheckResult propDCommunative

{------------------------------------------------------------------------------
    Assignment 6
    Implementing and testing ROT13 encoding
    Time: 60 min

    Status: Done
------------------------------------------------------------------------------}

-- Shift alphabetic value x places up
cipher :: String -> Int -> String
cipher [] shift = []
cipher (x:xs) shift
    -- Trick is to cast alphabetic chars to corresponding ints and back
    | x `elem` ['A'..'Z'] = chr (((ord x - ord 'A' + shift) `mod` 26) + ord 'A') : cipher xs shift
    | x `elem` ['a'..'z'] = chr (((ord x - ord 'a' + shift) `mod` 26) + ord 'a') : cipher xs shift
    | otherwise = x : cipher xs shift

rot13 :: String -> String
rot13 s = cipher s 13

-- cases
-- ROT13 is its own inverse; that is, to undo ROT13
-- 1) rot13 (rot13 xs) = xs
-- 2) profit???
propRot13 :: String -> Bool
propRot13 xs = xs == rot13 (rot13 xs)

assignment6 = quickCheck propRot13

{------------------------------------------------------------------------------
    Assignment 7
    Implementing and testing IBAN validation
    Time: 120 min
------------------------------------------------------------------------------}

countryFormat = [("BE", 16), ("FR", 27), ("DE", 22), ("GR", 27)]

-- Iban testcases from wikipedia
ibanCorrect, ibanFalse :: [String]
ibanCorrect = ["BE71096123456769","FR7630006000011234567890189","DE91100000000123456789","GR9608100010000001234567890"]
-- did everything +1, lazy!
ibanFalse = ["BE71096123456770","FR7630006000011234567890190","DE91100000000123456790","GR9608100010000001234567891"]

-- convert [Int] to int
-- source https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
flattenNumbers :: [Int] -> Integer
flattenNumbers = read . concatMap show

-- Replace each letter in the string with two digits, 
-- thereby expanding the string, where A = 10, B = 11, ..., Z = 35
digitizeLetters :: String -> [Int]
digitizeLetters [] = []
digitizeLetters (x:xs)
    | isLetter x = ord (toLower x) - 87 : digitizeLetters xs
    | otherwise  = digitToInt x : digitizeLetters xs 

-- Check if iban is valid number
validateIban :: String -> Bool
validateIban xs = (flattenNumbers (digitizeLetters (drop 4 xs ++ (take 4 xs))) `mod` 97 == 1)

-- Check if the length of the iban correspondends with the country formatting
validateCountry :: String -> Bool
validateCountry xs = length xs == fromMaybe 0 (lookup (take 2 xs) countryFormat) 

-- Check if input is valid Iban
iban :: String -> Bool
iban xs = validateIban xs && validateCountry xs

-- Pathetic quickcheck attempt.
assignment7 :: IO Result
assignment7 = do
    quickCheckResult (all (==True) (map iban ibanCorrect))
    quickCheckResult (all (==False) (map iban ibanFalse))
