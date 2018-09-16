module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
                p <- getStdRandom random
                ps <- probs (n-1) 
                return (p:ps)
    
-- Lab 2.1
countFloatQuartiles :: [Float] -> [Int] -> [Int]
countFloatQuartiles [] res = res
countFloatQuartiles (y:ys) res 
    | y >= 0.0 && y < 0.25 = countFloatQuartiles ys (1:res)
    | y >= 0.25 && y < 0.5 = countFloatQuartiles ys (2:res)
    | y >= 0.5 && y < 0.75 = countFloatQuartiles ys (3:res)
    | y >= 0.75 && y < 1 = countFloatQuartiles ys (4:res)

countInt :: Int -> [Int] -> Int -> Int
countInt n [] r = r
countInt n (y:ys) r = if n == y 
                      then countInt n ys (r+1)
                      else countInt n ys r

startCountingQuartiles :: Int -> IO [Int]
startCountingQuartiles n = do 
                            xs <- (probs n)
                            let xr = countFloatQuartiles xs []
                            let x1 = countInt 1 xr 0
                            let x2 = countInt 2 xr 0
                            let x3 = countInt 3 xr 0
                            let x4 = countInt 4 xr 0
                            return [x1, x2, x3, x4]

-- Lab 2.2
data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = (a + b) < c ||
                     (c + a) < b ||
                     (b + c) < a

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && b == c

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = (a^2 + b^2) == c^2 || 
                      (b^2 + c^2) == a^2 || 
                      (c^2 + a^2) == b^2

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = a == b || b == c || c == a

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | isNoTriangle a b c = NoTriangle
    | isEquilateral a b c = Equilateral
    | isRectangular a b c = Rectangular
    | isIsosceles a b c = Isosceles
    | otherwise = Other

-- Lab 2.3
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

biggerThanThree :: Int -> Bool
biggerThanThree x = x > 3

integerDomain23 = [-10..10]
domain23 :: [Int]
domain23 = [fromInteger x | x <- integerDomain23]

propEven = even

prop1 :: Int -> Bool
prop1 x = even x && biggerThanThree x

prop2 :: Int -> Bool
prop2 x = even x || biggerThanThree x

prop3 :: Int -> Bool
prop3 x = (even x && biggerThanThree x) || even x

prop4 :: Int -> Bool
prop4 x = (even x && biggerThanThree x) || even x

-- Lab 2.3.2
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = elem a (permutations b)

sameLength :: [a] -> [a] -> Bool
sameLength xs ys = length xs == length ys

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = all (\ x -> x `elem` xs) ys

-- Lab 2.4
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True 
isDerangement (x:xs) (y:ys) = not (x == y) && isDerangement xs ys 

deran :: Eq a => [a] -> [[a]]
deran lst = filter (\ x -> isDerangement lst x) (permutations lst)

-- Lab 2.5
rotateBy :: [Char] -> [Char] -> Int -> [Char]
rotateBy [] res n = res
rotateBy (x:xs) res n = rotateBy xs (res++[y]) n where y = chr ((ord x) + n)

rot13 :: [Char] -> [Char]
rot13 str = rotateBy str "" 13

-- Lab 2.6
-- Only allowing dutch length
checkIbanLength :: String -> Bool
checkIbanLength str = length str == 18

moveToEnd :: String -> Int -> String
moveToEnd (x:xs) 1 = xs++[x]
moveToEnd (x:xs) n = moveToEnd (xs++[x]) (n-1)

letterReplacer :: Char -> [Char]
letterReplacer c
    | x >= 65 && x <= 90 = show (x - 55) 
    | otherwise = [c]
    where x = ord c

checkRemainder :: String -> Bool
checkRemainder str = (mod (read (concatMap letterReplacer (moveToEnd str 4))::Integer) 97) == 1

iban :: String -> Bool
iban str = checkIbanLength str && checkRemainder str