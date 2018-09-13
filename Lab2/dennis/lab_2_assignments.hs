module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


{------------------------------------------------------------------------------

  Assignment 1
  Your programmer Red Curry...

  Hours spent: 1
  Answers:
  After running a few test with n = 100000, it seems that the claim that the probs function is random, is correct,
  although, this may need to be verified with a statistical test.

  Outputs:
[24874,25093,25048,24985]
[24898,24829,25219,25054]
[25207,25160,24757,24876]
[24813,25106,25078,25003]
[25131,25000,25047,24822]

------------------------------------------------------------------------------}
getFloatsInInterval :: Float -> Float -> [Float] -> [Float]
getFloatsInInterval _ _ [] = []
getFloatsInInterval min max xs = filter (\x -> x >= min && x < max) xs

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

testProbs :: Int -> IO [Int]
testProbs n = do
            xs <- probs n
            let x1 = getFloatsInInterval 0.0 0.25 xs
            let x2 = getFloatsInInterval 0.25 0.5 xs
            let x3 = getFloatsInInterval 0.5 0.75 xs
            let x4 = getFloatsInInterval 0.75 1 xs
            return [length x1, length x2, length x3, length x4]

{------------------------------------------------------------------------------

  Assignment 2
  Recognizing triangles

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

equilateral :: Integer -> Integer -> Integer -> Bool
equilateral a b c = a == b && b == c



triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = NoTriangle

{------------------------------------------------------------------------------

  Assignment 3
  Testing properties strength

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
--stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
--stronger xs p q = forall xs (\ x -> p x --> q x)
--weaker   xs p q = stronger xs q p
-- TODO

{------------------------------------------------------------------------------

  Assignment 4
  Recognizing Permutations

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO

{------------------------------------------------------------------------------

  Assignment 5
  Recognizing and generating derangements

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO

{------------------------------------------------------------------------------

  Assignment 6
  Implementing and testing ROT13 encoding

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO

{------------------------------------------------------------------------------

  Assignment 7
  Implementing and testing IBAN validation

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO







--

