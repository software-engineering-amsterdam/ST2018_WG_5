import Lab5refac

-- Assignment 5
-- Hours: 1

assignment5 = do
    s <- genRandomSudokuNrc
    n <- genProblemNrc s
    showNode n
    solveShowNsNrc [n]