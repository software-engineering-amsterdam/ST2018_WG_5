module Assignment4 where

import RefactoredSource

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 3
  Answer:
    Taking into account that the criteria for a legit sudoku is that there is 1 solution for it.

    It is very rare to find a sudoku with 4 empty blocks. 3 empty blocks is easy to find and the program below
    can do so.
    5 seems impossible
------------------------------------------------------------------------------}
blocks' = map (\(x,y) -> [(a,b) | a <- x, b <- y]) [(x,y) | x <- blocks, y <- blocks]

-- Clear a block in the Node
removeBlock :: Node -> [(Row,Column)] -> Node
removeBlock node [] = node
removeBlock node (x:xs) = removeBlock (eraseN node x) xs

-- Clear a list of blocks in the node
removeBlocks :: Node -> [[(Row,Column)]] -> Node
removeBlocks node [] = node
removeBlocks node (x:xs) = removeBlocks (removeBlock node x) xs

-- Take a slice out of a list from a start point to an end point
slice from to xs = take (to - from + 1) (drop from xs)

-- Returns all possibilities for stripping n blocks from a node
stripNBlocks :: Node -> Int -> Int -> [[(Row,Column)]] -> [Node]
stripNBlocks node 0 n [] = []
stripNBlocks node start n blocks
        | (start + n) > (length blocks) = []
        | otherwise = (removeBlocks node (slice start (start + (n-1)) blocks)) : (stripNBlocks node (start + 1) n blocks)

assignment4 n x
        | x == 0 = print ("Done")
        | otherwise = do
                sud <- genRandomSudoku
                let stripped = stripNBlocks sud 0 n blocks'
                let nodes = (filter (\x -> (uniqueSol x)) stripped)
                if null nodes then print ("No valid sudoku possible") else showNode (head nodes)
                assignment4 n (x-1)
{- Example output for performing the test 3 times for 3 empty blocks:
    *Lab5> assignment4 3 3
    +-------+-------+-------+
    | 9 2 4 | 3 5 1 | 7 8 6 |
    | 3 1 6 | 4 7 8 | 9 5 2 |
    | 7 8 5 | 2 9 6 | 3 1 4 |
    +-------+-------+-------+
    | 4 5 2 | 7 6 9 |       |
    | 6 3 8 | 1 2 5 |       |
    | 1 7 9 | 8 3 4 |       |
    +-------+-------+-------+
    |       |       | 5 7 9 |
    |       |       | 8 2 3 |
    |       |       | 6 4 1 |
    +-------+-------+-------+
    "No valid sudoku possible"
    +-------+-------+-------+
    | 9 2 7 |       |       |
    | 6 8 4 |       |       |
    | 1 3 5 |       |       |
    +-------+-------+-------+
    |       | 9 5 2 | 8 7 4 |
    |       | 4 1 3 | 2 6 9 |
    |       | 6 8 7 | 1 3 5 |
    +-------+-------+-------+
    | 5 7 1 | 8 3 4 | 6 9 2 |
    | 4 9 3 | 7 2 6 | 5 1 8 |
    | 8 6 2 | 1 9 5 | 3 4 7 |
    +-------+-------+-------+
    "Done"
-}