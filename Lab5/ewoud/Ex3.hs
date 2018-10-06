module Ex3 where

import Data.List
import System.Random
import Lecture5
--import Lecture5Nrc
--import Lecture5Refactor

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 1h

------------------------------------------------------------------------------}
isMinimalized :: Node -> [(Row,Column)] -> Bool
isMinimalized n [] = uniqueSol n
isMinimalized n ((r,c):rcs) 
    | uniqueSol (eraseN n (r,c)) = False
    | otherwise = isMinimalized n rcs

-- ghetto quickcheck, kon geen echte quickcheck verzinnen zonder generator/korte tests
prop_isMinimalized :: Int -> IO ()
prop_isMinimalized 0  = print ("Generator is minimal")
prop_isMinimalized x = do
    print ("prrrrrt test")
    r <- Lecture5.genRandomSudoku
    s <- Lecture5.genProblem r
    if (isMinimalized s $ filledPositions (fst s))
        then prop_isMinimalized (x - 1)
        else print ("FAIL Generator is NOT minimal")

-- example
ass3 :: IO()
ass3 = do
    r <- Lecture5.genRandomSudoku
    s <- Lecture5.genProblem r
    let pmin = isMinimalized s $ filledPositions (fst s)
    print (pmin)

test3 = prop_isMinimalized 3