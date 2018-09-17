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

  Hours spent: 0.5
  Answers:

------------------------------------------------------------------------------}
data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

equilateral :: Integer -> Integer -> Integer -> Bool
equilateral a b c = a == b && b == c

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | (a + b < c) || (a + c < b) || (b + c < a) = NoTriangle
    | a == b && b == c = Equilateral
    | (a^2 + b ^2 == c^2) || (a^2 + c^2 == b^2) || (c^2 + b^2 == a^2)  = Rectangular
    | a == b || b == c || a == c = Isosceles
    | otherwise = Other

{------------------------------------------------------------------------------

  Assignment 3
  Testing properties strength

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker   xs p q = stronger xs q p

p1 :: Int -> Bool
p1 x = even x && x > 3

p2 :: Int -> Bool
p2 x = even x || x > 3

p3and4 :: Int -> Bool
p3and4 x = (even x && x > 3) || even x

--getDescendingPropertyList ::


{------------------------------------------------------------------------------

  Assignment 4
  Recognizing Permutations

  Hours spent: TODO
  Answers:
Not containing duplicates in the test lists, we can define the following test sets, such as:
:q
[] []
[1] []
[] [1]
[1..10] [1..10]
[1..4] (take 1 (permutations [1..4]))!!0

*Lab2> stronger (zip (permutations [1..4]) (permutations [1..4])) isSameLength containsSameElements
True
*Lab2> stronger (zip (permutations [1..4]) (permutations [1..4])) containsSameElements isSameLength
True
*Lab2>  stronger (zip (permutations [1..4]) (permutations [1..8])) containsSameElements isSameLength
False
*Lab2>  stronger (zip (permutations [1..4]) (permutations [1..8])) isSameLength containsSameElements
True
*Lab2>  stronger (zip (permutations [1..4]) (permutations [5..8])) containsSameElements isSameLength
True
*Lab2>  stronger (zip (permutations [1..4]) (permutations [5..8])) isSameLength containsSameElements
False

With different domains of input the 2 properties have different results. This states that the 2 properties have (partially) disjoint sets of input to which they apply,
because of this, the properties have no ordering, as they are equal.
------------------------------------------------------------------------------}

isPermutation :: Eq a => ([a],[a]) -> Bool
isPermutation (xs,ys) = if length xs == length ys then all (\x -> x `elem` ys) xs else False

isSameLength :: Eq a => ([a],[a]) -> Bool
isSameLength (xs,ys) = length xs == length ys

containsSameElements :: Eq a => ([a],[a]) -> Bool
containsSameElements (xs,ys) = all (\x -> x `elem` ys) xs

{------------------------------------------------------------------------------

  Assignment 5
  Recognizing and generating derangements

  Hours spent: TODO
  Answers:

  Properties for deran:
  x[i] != y[i]
  length x == length y

  Integer test lists for deran:
  []
  [1,2,3]
  [1,2,3,4,5]
  [1,2,3,4,5,6]


------------------------------------------------------------------------------}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = if x == y then False else isDerangement xs ys

deran :: [a] -> [a] -> [a]
deran [] res = res
deran [x] res = [x]++res
deran (x:y:xs) res = deran xs (res++[y,x])



{------------------------------------------------------------------------------

  Assignment 6
  Implementing and testing ROT13 encoding

  Hours spent: 0.5
  Answers:
--

------------------------------------------------------------------------------}

charRot13 :: Char -> Char
charRot13 c | ord c < ord 'a' || ord c > ord 'z' = error "Not a lowercase char!"
            | ord c <= ord 'm' = chr (ord c + 13)
            | otherwise = chr (ord 'a' + (13 - (ord 'z' + 1  - ord c)))

rot13 :: [Char] -> [Char]
rot13 xs = map (charRot13) xs

prop_checkRot13 xs = xs == rot13 (rot13 xs)
  where types xs = xs::[Char]

{------------------------------------------------------------------------------

  Assignment 7
  Implementing and testing IBAN validation

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}

isCapitalLetter :: Char -> Bool
isCapitalLetter x = ord x >= ord 'A' && ord x <= ord 'Z'

moveCharactersToEnd :: String -> Int -> String
moveCharactersToEnd xs x = snd res ++ fst res where res = splitAt x xs

capitalLetterToDigit :: Char -> [Char]
capitalLetterToDigit x | isCapitalLetter x == False = error "Not a capital letter!"
                       | otherwise            = show (ord x - 55)

replaceLettersByDigits :: String -> String
replaceLettersByDigits xs = concat (map (\x -> if isCapitalLetter x then capitalLetterToDigit x else [x]) xs)

iban :: String -> Bool
iban xs = mod (read(replaceLettersByDigits (moveCharactersToEnd xs 4))) 97 == 1


--

