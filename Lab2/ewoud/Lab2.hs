
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Function
import System.Random

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


{-|
    Assignment x
    Time: x min


-}

{-|
    Assignment 1
    Time: 90 min

    STATE: Done, but perhaps better print because this is uglyyyyyyy
-}

-- use ghetto numpy digitize to "bin" values into range!
binValue :: Float -> Int
binValue x 
    | x > 0 && x < 0.25 = 1
    | x >= 0.25 && x < 0.50 = 2
    | x >= 0.50 && x < 0.75 = 3
    | x >= 0.75 && x < 1 = 4
    | otherwise = 0

-- Chi square test
-- Zie dobbelsteen voorbeeld wikipedia
-- https://nl.wikipedia.org/wiki/Chi-kwadraattoets

chiSquare :: Int -> [Int] -> Float
chiSquare n xs = fromIntegral (sum [(x - n)^2 | x <- xs]) / (fromIntegral (length xs) / fromIntegral (length(nub xs)))

-- Source;
-- https://codereview.stackexchange.com/questions/139587/coufromIntegral 
numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

-- Null hypothesis = random generator is random!
-- p value = 0.05, DF = 3, x^2 = 7.815
-- pValue = 0.05
chiDistr = 7.815
doAssign1 = assignment1 10000

assignment1 :: Int -> IO Float--IO [Integer]
assignment1 x = do
    -- generate numbers
    genlist <- probs x
    -- bin numbers into quartile
    let binned = [binValue x | x <- (genlist)]
    let bincount = [numTimesFound x binned | x <- [1..4]]
    let pvar = chiSquare x bincount
    -- UGLY, do it better...
    if chiDistr > pvar
        then putStrLn ("not random") 
        else putStrLn ("random!") 
    return pvar


{-|
    Assignment 2 
    Recognizing triangles
    Time: 90 min

    Status: Done? Missing is the quickcheck for other cases
-}

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


testx :: Integer -> Integer -> Integer
testx a b = floor(sqrt(fromIntegral (a^2 + b^2)))
-- gen triangles
-- 5 types?
assignment2 :: IO Bool--IO [Integer]
assignment2 = do
    -- No triangle!
    quickCheckResult (\ (Positive a) (Positive b) (Positive c) -> triangle a b (a + b + c) == NoTriangle)
    -- Equilateral, same length
    quickCheckResult (\ (Positive a) -> triangle a a a == Equilateral)
    -- Rectangular (90°), using Euclid's formula
    quickCheckResult (\ (Positive m) (Positive n) -> (m > n) --> triangle (m ^ 2 - n ^ 2)(2 * m * n)(m ^ 2 + n ^ 2) == Rectangular)
    -- isocles, check if triangle and generate non equilateral cases
    quickCheckResult (\ (Positive m) (Positive n) -> (n <= m + m && m /= n) --> triangle m m n == Isosceles)
    -- Other ( i dont know what to do about this...)
    return True

{-|
    Assignment 3 
    Testing properties strength
    Time: x min

    Status: wtf is deze ik snap het niet omg???
-}

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker   xs p q = stronger xs q p

-- properties workshopa 3a
test1, test2, test3, test4 :: Int -> Bool
test1 x = even x && x > 3
test2 x = even x || x > 3
test3 x = (even x && x > 3) || even x
test4 x = (even x && x > 3) || even x

prop1, prop2, prop3, prop4 :: Bool
prop1 = stronger [(-10)..10] test1 even
prop2 = stronger [(-10)..10] test2 even
prop3 = stronger [(-10)..10] test3 even
prop4 = stronger [(-10)..10] even test4

{-|
    Assignment 4 
    Recognizing Permutations
    Time: 45 min
        
    Status: Functional
    Todo: make quickechk pretty (spaghetti logic atm)
-}

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

--
-- TODO MAKE pretty
-- Works but ugly!
-- i check the length of the intersection of 2 unique lists 
-- with both individual lists. 
-- If they are equal we have a a permutation because we may assume that there are no duplicates in the list
-- and thus have a 1:1 image domain.
propIsPermutation :: [Integer] -> [Integer] -> Bool
propIsPermutation xs ys = isPermutation (nub xs) (nub ys) == bijective
    where bijective = length (nub xs) == length (intersect (nub xs) (nub ys)) && length (nub ys) == length (intersect (nub xs) (nub ys))

assignment4 = quickCheck propIsPermutation

{-|
    Assignment 5 
    Recognizing and generating derangements
    Time: 
-}

isDerangement :: Eq a => [a] -> [a] -> Bool
isDenormalized xs ys = False
{-|
    Assignment 6
    Implementing and testing ROT13 encoding
    Time: 60 min

    Status: Done
-}

-- Shift alphabetic value x places up
cipher :: String -> Int -> String
cipher [] shift = []
cipher (x:xs) shift
    -- is this readable?
    -- Trick is to cast alphabetic chars to corresponding ints and back
    | x `elem` ['A'..'Z'] = chr (((ord x - ord 'A' + shift) `mod` 26) + ord 'A') : cipher xs shift
    | x `elem` ['a'..'z'] = chr (((ord x - ord 'a' + shift) `mod` 26) + ord 'a') : cipher xs shift
    | otherwise = x : cipher xs shift

rot13 :: String -> String
rot13 s = cipher s 13

-- cases
-- 1) ROT13 is its own inverse; that is, to undo ROT13
-- 1) rot13 (rot13 xs) = xs
-- 2) ???
propRot13 :: String -> Bool
propRot13 xs = xs == rot13 (rot13 xs)

assignment6 = quickCheck propRot13


{-|
    Assignment 7
    Implementing and testing IBAN validation
    Time: 60 min
-}