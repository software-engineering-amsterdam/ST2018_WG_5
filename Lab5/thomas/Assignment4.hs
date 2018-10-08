import Lab5

emptyBlock :: Node -> [(Row,Column)] -> Node
emptyBlock n [] = n
emptyBlock n (rc:rcs) = emptyBlock (eraseN n rc) rcs

removeBlocks :: Node -> [(Row,Column)] -> Node
removeBlocks n [] = n
removeBlocks n ((r,c):rcs) = removeBlocks newNode rcs
    where newNode = emptyBlock n [(r',c') | r' <- bl (r * 3), c' <- bl (c * 3)]

generateEmptyBlockSudoku :: Int -> IO Node
generateEmptyBlockSudoku num = do r <- genRandomSudoku
                                  let allBlocks = [(r,c) | r <- [1..3], c <- [1..3]]
                                  ranBlocks <- randomize allBlocks
                                  let numRanBlocks = take num ranBlocks
                                  let s = removeBlocks r numRanBlocks
                                  let z = minimalize s (filledPositions (fst s))
                                  return z

keepGenerating :: Int -> IO Node                                  
keepGenerating num = do z <- generateEmptyBlockSudoku num
                        if uniqueSol z 
                        then return z
                        else keepGenerating num

-- Assignment 4
-- Hours: 1.5
-- Answers: Generating for four empty blocks still works, five empty blocks does not

assignment4 :: Int -> IO ()
assignment4 num = do z <- keepGenerating num
                     print (uniqueSol z)
                     showNode z