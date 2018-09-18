module Lab2 where

import Data.List
import Data.Char
import Data.Function
import System.Random
import Test.QuickCheck
import Data.Maybe
import Text.Show.Functions

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

-- Chi square test
-- See dice example for understanding: https://nl.wikipedia.org/wiki/Chi-kwadraattoets
chiSquare :: Int -> [Int] -> Float
chiSquare n xs = fromIntegral (sum [(x - n)^2 | x <- xs]) / (fromIntegral (length xs) / fromIntegral (length(nub xs)))

assignment1 :: Int -> IO Float
assignment1 x = do
    -- generate numbers
    genlist <- testProbs x
    -- bin numbers into quartile
    let bincount = genlist
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

  Hours spent: 1
  Answers:
  - program
    - Isosceles (gelijkbenig) 2 or 3 sides should be equal, so just check the minimum,
      being 2 equal sides (a,b), (b,c) or (c,a)
    - Rectangular (rechthoekig) 2 sides to the power of 2 added with eachother should
      become the 3rd side to the power of 2. Also here every case should be checked.
      a^2 + b^2 = c^2?, c^2 + a^2 = b^2?, b^2 + c^2 = a^2? If any matches, it's a rectangular.
    - Equilateral (Gelijkzijdig) mean all sides are the same. This should be checked before
      the isosceles, otherwise the weaker statements is caught instead of this stronger
      statement. Equilateral --> Isosceles (and this can't be reversed), so equilateral is stronger
    - Not a triangle (Geen driehoek) is determined as (a >= (b+c) || c >= (b+a) || b >= (a+c))
      and should be checked on first, since if this is the case, every other check is pointless
  - Testing
    - Testing this with for example quick check generating values and based on that creating types of triangles
      is not the way to go. (I.E. generate a and b and create a test triangle of a a b to check for isosceles)
      This is not the way to go since you are kind of reversing the logic you wrote in the first place for
      finding out the type.
    - The best way to go is creating a bunch of triangles in which we (the oracle) know what the outcome should be

------------------------------------------------------------------------------}
data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | (a >= (b+c) || c >= (b+a) || b >= (a+c)) = NoTriangle
    | a == b && b == c && a == c = Equilateral
    | a^2 + b^2 == c^2 = Rectangular
    | c^2 + a^2 == b^2 = Rectangular
    | b^2 + c^2 == a^2 = Rectangular
    | a == b || b == c || a == c = Isosceles
    | otherwise = Other

assignment2 = [
  all (\x -> x == NoTriangle) [triangle 2 2 4, triangle 2 3 99, triangle 12 45 2],
  all (\x -> x == Equilateral)  [triangle 1 1 1, triangle 4 4 4],
  all (\x -> x == Rectangular)  [triangle 3 4 5, triangle 5 12 13, triangle 7 24 25],
  all (\x -> x == Isosceles)  [triangle 5 5 8, triangle 5 5 6],
  all (\x -> x == Other)  [triangle 2 3 4, triangle 4 5 6]
  ]
  -- [True,True,True,True,True]

{------------------------------------------------------------------------------

  Assignment 3
  Testing properties strength

  Hours spent: 0.5
  Answers:
  - The result is "acdb" in which the test 1 to 4 range from 'a' to 'd'

------------------------------------------------------------------------------}
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker   xs p q = stronger xs q p

ass3A = ((\ x -> even x && x > 3), 'a')
ass3B = ((\ x -> even x || x > 3), 'b')
ass3C = ((\ x -> (even x && x > 3) || even x), 'c')
ass3D = ((\ x -> (even x && x > 3) || even x), 'd')
ass3All = [ass3A, ass3B, ass3C, ass3D]

sortProp :: ((Int -> Bool), Char) -> ((Int -> Bool), Char) -> Ordering
sortProp (x, _) (y, _)
  |  (stronger [(-10)..10] y x) && (weaker [(-10)..10] y x) = EQ
  | stronger [(-10)..10] y x = GT
  | weaker [(-10)..10] y x = LT
ass3Sorted = map (\x -> snd x) (sortBy sortProp ass3All)
-- "acdb"

{------------------------------------------------------------------------------
    Assignment 4
    Recognizing Permutations
    Time: 45 min

    Answers:
    - The 2 properties to which a permutation must hold is: same length and a check
      if every element is contained in the other
      - These 2 are equally strong

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
-- +++ OK, passed 100 tests.

{------------------------------------------------------------------------------

  Assignment 5
  Recognizing and generating derangements

  Hours spent: 1h
  Answers:
  - So derangement is the same as a permutation with the additional property:
    "Nothing can be on the same position as the original"
  - The new "notOnSamePositions" is just as strong as the "isPermutation" property
    - Output:
    *Lab2> stronger ass5Tests notOnSamePositions isPermutation
          False
    *Lab2> stronger ass5Tests isPermutation notOnSamePositions
          False

------------------------------------------------------------------------------}
deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement (list, x)) (permutations list)
  where list = [0..(n-1)]

deranFromList :: [Int] -> [[Int]]
deranFromList list = filter (\x -> isDerangement (list, x)) (permutations list)

isDerangement :: Eq a => ([a],[a]) -> Bool
isDerangement lists = isPermutation (fst lists) (snd lists) && notOnSamePositions lists

notOnSamePositions :: Eq a => ([a],[a]) -> Bool
notOnSamePositions (xs, ys) = all (\x -> x) [(xs !! x) /= (ys !! x) | x <- [0..((length shortestList) - 1)]]
  where
    shortestList = if length xs <= length ys then xs else ys

ass5Valids  = [([2,1,3], [3,2,1])]
ass5Invalids  = [([1,2,3], [3,1,2,4]), ([1,2,3], [3,1,1]), ([1,2,3], [3,1,9]), ([1,2,3], [3,2,1])]

ass5BasicTest = (map (\x -> isDerangement x) ass5Valids) ++ (map (\x -> not (isDerangement x)) ass5Invalids)
-- [True,True,True,True,True]
ass5Test :: [Int] -> Bool
ass5Test list = all (\x -> x) (map (\x -> isDerangement (x, list)) (deranFromList list))
-- quickCheck ass4Test
-- (13 tests).... and running (slow because of big lists being generated)

{------------------------------------------------------------------------------

  Assignment 6
  Implementing and testing ROT13 encoding

  Hours spent: 1
  Answers:
  - The specification for ROT13 is that it obfuscates a text and that by applying
    the function twice, the result is the original text
      - So performing the function twice on the input should also yield the input
  - The implementation only takes lower cased string into account and leaves others as is

------------------------------------------------------------------------------}
alfabet = ['a'..'z']

alfabetIndex char = findIndex (\x -> x == char) alfabet

performShift ::  Maybe Int -> Char -> Char
performShift (Just index) _ = alfabet !! ((index + 13) `mod` 26)
performShift (Nothing) oldChar = oldChar

rot13 :: [Char] -> [Char]
rot13 text = map (\char -> (performShift (alfabetIndex char) char)) text

propRot13 :: [Char] -> Bool
propRot13 text =  text == rot13 (rot13 text)

assignment6 = quickCheck propRot13
-- +++ OK, passed 100 tests.

{------------------------------------------------------------------------------

  Assignment 7
  Implementing and testing IBAN validation

  Hours spent: 2
  Answers:
  - Created the check step by step.
    - Main resource: https://codereview.stackexchange.com/questions/135366/python-iban-validation
  - I've tested the solution with a list of valid and invalid ibans (see below)
  - It's not really possible to completely automate the test process. It might be possible
    by creating a function that generates valid/invalid ibans and testing if the written
    method reports correctly on those. Altough then you should also test this method which creates
    IBANs, so it's a vicious circle.
  - We could extend the countries supported easily by adding it's rules to the list of supported countries

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

assignment7 = (map iban ibanCorrect) ++ (map (\x -> not (iban x)) ibanFalse)
--[True,True,True,True,True,True,True,True]
