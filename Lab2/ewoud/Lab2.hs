
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Function

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
    Time: x min

    
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

chiSquare :: Integer -> [Integer] -> Float
chiSquare n xs = fromIntegral (sum [(x - n)^2 | x <- xs]) / (fromIntegral n)

-- Source;
-- https://codereview.stackexchange.com/questions/139587/count-occurrences-of-an-element-in-a-list
numTimesFound :: Ord a => a -> [a] -> Integer
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

--tmp = [1,1,1,2,3,3,3,4]
--testme = [numTimesFound x tmp | x <- [1..4]]

assignment1 :: Int -> IO Float --IO [Integer]
assignment1 x = do
    -- generate numbers
    genlist <- probs x
    -- bin numbers into quartile
    let binned = [binValue x | x <- (genlist)]
    let bincount = [numTimesFound x binned | x <- [1..4]]
    let pvar = chiSquare 3 bincount
    -- do some statistics/quickcheck?
    return pvar

{-|
    Assignment 2 
    Time: x min


-}

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    -- Not a triangle (non 2d bs), need wiki source?
    | (a + b < c) || (a + c < b) || (b + c < a) = NoTriangle
    -- An equilateral triangle has all sides the same length
    | a == b && b == c = Equilateral
    -- A rectangled triangle has one of its interior angles measuring 90°.
    | a ^ 2 + b ^2 == c ^ 2 = Rectangular
    -- An isosceles triangle has two sides of equal length.
    | a == b || b == c || a == c = Isosceles
    -- Other
    | otherwise = Other

-- gen triangles
-- 5 types?

{-|
    Assignment 3 
    Time: x min


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

{-|
    Assignment 4 
    Time: x min


-}

isDerangement :: (Ord a) => [a] -> [a] -> Bool
isDerangement a b = False

--deran