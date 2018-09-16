module Lab2 where

import Data.List
import Data.Char
import Data.Function
import System.Random
import Test.QuickCheck
import Text.Show.Functions

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


{------------------------------------------------------------------------------

  Assignment 1
  Your programmer Red Curry...

  Hours spent: 1.5
  Answers:
  - Based on the output, it seems to be quite random
  - To really make sure that this is in fact unbiased. A chi-squared test can be performed
    https://en.wikipedia.org/wiki/Pearson%27s_chi-squared_test

------------------------------------------------------------------------------}
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

halfPartition n = do
  originalList <- probs n
  return  (partition (\x -> x >= 0.0 && x < 0.5) originalList)

quarterPartitions n = do
  halves <- halfPartition n
  return ((partition (\x -> x >= 0.0 && x < 0.25) (fst halves)), (partition (\x -> x >= 0.5 && x < 0.75) (snd halves)))

countOccurences = do
  quarters <- quarterPartitions 10000
  return [length (fst (fst quarters)), length (snd (fst quarters)), length (fst (snd quarters)), length (snd (snd quarters))]

assignment1 = countOccurences
-- Some example output [2364,2558,2548,2530], [2443,2545,2572,2440], [2467,2487,2540,2506]

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

tests = [
  all (\x -> x == NoTriangle) [triangle 2 2 4, triangle 2 3 99, triangle 12 45 2],
  all (\x -> x == Equilateral)  [triangle 1 1 1, triangle 4 4 4],
  all (\x -> x == Rectangular)  [triangle 3 4 5, triangle 5 12 13, triangle 7 24 25],
  all (\x -> x == Isosceles)  [triangle 5 5 8, triangle 5 5 6],
  all (\x -> x == Other)  [triangle 2 3 4, triangle 4 5 6]
  ]
  -- TODO add output


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
-- TODO add output

{------------------------------------------------------------------------------

  Assignment 4
  Recognizing Permutations

  Hours spent: 2h
  Answers:
  - The 2 properties to which a permutation must hold is: same length and check if everySecond
    element is contained in the other
    - These 2 are equally strong
      Output:
      *Lab2> stronger ass4Tests isSameLength containsAll
              False
      *Lab2> stronger ass4Tests containsAll isSameLength
              False
  - The correct domain of test cases must also include exceptions for both properties


------------------------------------------------------------------------------}
isPermutation :: Eq a => ([a],[a]) -> Bool
isPermutation lists = isSameLength lists && containsAll lists

isSameLength :: ([a],[a]) -> Bool
isSameLength (xs,ys) = (length xs) == (length ys)

containsAll :: Eq a => ([a],[a]) -> Bool
containsAll ([],_) = True
containsAll ((x:xs),ys) = elem x ys && containsAll (xs,ys)

ass4Valids  = [([1,2,3], [3,2,1])]
ass4Invalids = [([1,2,3], [3,2,1,4]), ([1,2,3], [1,1,3])]

ass4 = (map (\x -> isPermutation x) ass4Valids) ++ (map (\x -> not (isPermutation x)) ass4Invalids)
-- TODO add output
ass4Test :: [Int] -> Bool
ass4Test list = all (\x -> x) (map (\x -> isPermutation (x, list)) (permutations list))
-- TODO add output

{------------------------------------------------------------------------------

  Assignment 5
  Recognizing and generating derangements

  Hours spent: 1h
  Answers:
  - So derangement is the same as a permutation with the additional property:
    Nothing can be on the same position as the original
  - The new "notOnSamePositions" is just as strong as the "isPermutation" property
    - Output:
    *Lab2> stronger ass5Tests notOnSamePositions isPermutation
          False
    *Lab2> stronger ass5Tests isPermutation notOnSamePositions
          False
    *Lab2>
  -

------------------------------------------------------------------------------}
deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement (list, x)) (permutations list)
  where list = [0..(n-1)]

deranFromList :: [Int] -> [[Int]]
deranFromList list = filter (\x -> isDerangement (list, x)) (permutations list)

isDerangement :: Eq a => ([a],[a]) -> Bool
isDerangement lists = isPermutation lists && notOnSamePositions lists

notOnSamePositions :: Eq a => ([a],[a]) -> Bool
notOnSamePositions (xs, ys) = all (\x -> x) [(xs !! x) /= (ys !! x) | x <- [0..((length shortestList) - 1)]]
  where
    shortestList = if length xs <= length ys then xs else ys

ass5Valids  = [([2,1,3], [3,2,1])]
ass5Invalids  = [([1,2,3], [3,1,2,4]), ([1,2,3], [3,1,1]), ([1,2,3], [3,1,9]), ([1,2,3], [3,2,1])]

ass5 = (map (\x -> isDerangement x) ass5Valids) ++ (map (\x -> not (isDerangement x)) ass5Invalids)
-- TODO add output
ass5Test :: [Int] -> Bool
ass5Test list = all (\x -> x) (map (\x -> isDerangement (x, list)) (deranFromList list))
-- TODO add output

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
-- *Lab2> assignment6
-- +++ OK, passed 100 tests.

{------------------------------------------------------------------------------

  Assignment 7
  Implementing and testing IBAN validation

  Hours spent: 1.5
  Answers:
  - Created the check step by step.
    - Main resource: https://codereview.stackexchange.com/questions/135366/python-iban-validation
  - I've tested the solution with a list of valid and invalid dutch ibans (see below)
  - It's not really possible to completely automate the test process. It might be possible
    by creating a function that generates valid/invalid ibans and testing if the written
    method reports correctly on those. Altough then you should also test this method which creates
    IBANs, so it's a vicious circle.
------------------------------------------------------------------------------}
iban :: String -> Bool
iban x = (isDutchIban x) && (ibanCheck x)

ibanCheck :: String -> Bool
ibanCheck iban = ((read (ibanToDigits (reformatIban iban)) :: Integer) `mod` 97)  == 1


-- First we start of by taking the country code and the check digit (first 4 chars)
-- and place them at the end of the iban
reformatIban :: String -> String
reformatIban iban = (drop 4 iban) ++ (take 4 iban)

-- Now use the charToDigit function to map every piece of the iban to one big iban number
-- containing only digits
ibanToDigits :: String -> String
ibanToDigits iban = concat (map (\x -> show (charToDigit x)) iban)

-- Every character must be replaced by 2 digits A = 10.. Z = 35
-- ord 'A' = 65, so we can use ord by substracting 55 whenever a char falls in the range of
-- capitalized characters
-- When this is not the case, substract 48 since that's where digits start
charToDigit :: Char -> Int
charToDigit char
  | ord char >= 65 = (ord char) - 55
  | otherwise = (ord char) - 48

-- Simple check for checking if iban can be a dutch iban
isDutchIban :: String -> Bool
isDutchIban iban = (hasLength iban 18) && ((take 2 iban) == "NL")

hasLength :: String -> Int -> Bool
hasLength string n = (length string) == n

validDutchIbans = ["NL92MHCB0632607123", "NL33AEGO0743850761", "NL78TRIO0397520348", "NL88FVLB0264875266"]
invalidDutchIbans = ["NL92MHCB02326071232342", "AR33AEGO0743250761", "NL78TRIO0327520348", "NL88FVLB0264275266"]
assignment7 = (map (\x -> iban x) validDutchIbans) ++ (map (\x -> not (iban x)) invalidDutchIbans)
-- *Lab2> assignment7
-- [True,True,True,True,True,True,True,True]
