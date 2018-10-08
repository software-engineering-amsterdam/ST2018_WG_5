import Lab5

filledPositionsN :: Node -> [(Row,Column)]
filledPositionsN n = filledPositions (fst n)

checkMinimalAfterPosition :: (Row,Column) -> Node -> Bool
checkMinimalAfterPosition position n = not (uniqueSol (eraseN n position))

recursePositions :: [(Row,Column)] -> Node -> Bool
-- recursePositions [] n = True
-- recursePositions (x:xs) n = checkMinimalAfterPosition x n && recursePositions xs n
recursePositions (x:xs) n = foldr (\x -> (&&) (checkMinimalAfterPosition x n)) True xs

isMinimal :: Node -> Bool
isMinimal n = uniqueSol n && recursePositions (filledPositionsN n) n


-- Assignment 3
-- Hours: 1.5
-- Answers: One could test if the generated solutions are minimal by checking if
-- there is a unique solution for the sudoku and if any of the hints is erased there
-- should not be a unique solution anymore.

assignment3 :: IO ()
assignment3 = do r <- genRandomSudoku
                 s <- genProblem r
                 print "is minimal check:"
                 print (isMinimal s)
                 