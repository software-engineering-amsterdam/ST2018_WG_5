module Assignment4 where

import Data.List
import System.Random
import Lecture5Assignment4

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 2
------------------------------------------------------------------------------}

-- recursive call to eraseN
eraseNodes :: Node -> [(Row,Column)] -> Node
eraseNodes n [] = n
eraseNodes n ((r,c):xs) = eraseNodes (eraseN n (r,c)) xs

-- Erase block i from Node
eraseBlock :: Node -> Int -> Node
eraseBlock n i = eraseNodes n [(r, c) | r <- blocks !! (i `quot` 3), c <- blocks !! (i `mod` 3)]

-- Erase X blocks from Node
setEmptyBlock :: Node -> [Int] -> Node
setEmptyBlock n [] = n
setEmptyBlock n (x:xs) = setEmptyBlock (eraseBlock n x) xs

genEmptyBlock :: Int -> IO Node
genEmptyBlock n = do
    ys <- genRandomSudoku
    b <- randsf n [] :: IO [Int] 
    let cs = setEmptyBlock ys b
    return cs

prop_emptyN :: Int -> Int -> Int -> IO()
prop_emptyN empty 0 n = putStrLn $ "Found " ++ show n ++ " possible sudokus with " ++ show empty ++ " empty blocks" 
prop_emptyN empty tests n = do
    sudoku <- genEmptyBlock empty
    if (uniqueSol sudoku)
        then prop_emptyN empty (tests - 1) (n + 1)
        else prop_emptyN empty (tests - 1) (n)


randsf :: (Eq a, Num a, Random a) => Int -> [a] -> IO [a]
randsf n rs
    | length rs == n = return rs -- 3 SHOULD BE n
    | otherwise = do
        r <- randomRIO (0,8)
        if elem r rs 
            then randsf n rs 
            else randsf n (r:rs)

-- prop_emptyN (empty blocks) (test attempts) (init value for tracking)
check3 = prop_emptyN 3 1000 0
-- Found 238 possible sudokus with 3 empty blocks
check4 = prop_emptyN 4 1000 0
-- Found 8 possible sudokus with 3 empty blocks
check5 = prop_emptyN 5 1000 0
-- Found 0 possible sudokus with 3 empty blocks