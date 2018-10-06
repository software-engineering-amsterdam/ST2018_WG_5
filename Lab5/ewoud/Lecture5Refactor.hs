
module Lecture5Refactor

where 

import Data.List
import System.Random
import Control.Monad
{--
  CHANGELOG  : assignment2
  changed (Row,Column) into Position (everywhere in this source)
  added new types position and constrnt
  added blocksNrc (assignment1)
  added constraint definitions
  added changed freeAtPos functionn
  added changed constraints function
  added changed prune function
  added changed connsistent function

  CHANGELOG  : assignment5
  Sorry lost track 

--}
type Position = (Row,Column)
type Constrnt = [[Position]]

blocksNrc :: [[Int]]
blocksNrc = [[2..4],[6..8]]

rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks]
nrcConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocksNrc, b2 <- blocksNrc]

gridConstr :: Constrnt
gridConstr = rowConstrnt ++ columnConstrnt ++ blockConstrnt

nrcGridConstr :: Constrnt
nrcGridConstr = gridConstr ++ nrcConstrnt

freeAtPos :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos s (r,c) xs = let
   ys = filter (elem (r,c)) xs
 in
   foldl1 intersect (map ((values \\) . map s) ys)

constraints :: Sudoku -> Constrnt -> [Constraint] 
constraints s cns = sortBy length3rd [(r,c, freeAtPos s (r,c) cns) | (r,c) <- openPositions s ]

-- added Constrnt
prune :: (Row,Column,Value) -> Constrnt -> [Constraint] -> [Constraint]
prune _ _ [] = []
prune (r,c,v) cns ((x,y,zs):rest)
  | (x,y) `elem` checkCons = (x,y,zs\\[v]) : prune (r,c,v) cns rest
  | otherwise = (x,y,zs) : prune (r,c,v) cns rest
  where checkCons = nub $ join $ filter (\x -> (r,c) `elem` x) cns 

consistent :: Sudoku -> Constrnt -> Bool
consistent s cns = all (\p -> p == nub p) [filter (/= 0) [s (r,c) | (r,c) <- cs] | cs <- cns]

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = Position -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> Position -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

subGrid :: Sudoku -> Position -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> Position -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> Position -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))
                    
extend :: Sudoku -> (Position,Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

-- added constrnt
-- renamed (s,xconstraints) to (s,ys)
extendNode :: Node -> Constrnt -> Constraint -> [Node]
extendNode (s,ys) cns  (r,c,vs) = 
  [(extend s ((r,c),v), sortBy length3rd $ prune (r,c,v) cns ys) | v <- vs ]
  
sameblock :: Position -> Position -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

-- added constrnt stuff
initNode :: Grid -> Constrnt -> [Node]
initNode gr cns = let s = grid2sud gr in 
              if (not (consistent s cns)) then [] 
              else [(s, constraints s cns)]

openPositions :: Sudoku -> [Position]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node 

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

-- 
solveNs :: [Node] -> Constrnt -> [Node]
solveNs xs cns = search (\x -> succNode x cns) solved xs -- source: michael

succNode :: Node -> Constrnt -> [Node]
succNode (s,[]) _ = []
succNode (s,p:ps) cns = extendNode (s,ps) cns p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr gridConstr) gridConstr

-- backwards compat, not required
solveAndShowCons :: Grid -> Constrnt -> IO[()]
solveAndShowCons gr cns = solveShowNs (initNode gr cns) cns

-- 
solveShowNs :: [Node] -> Constrnt -> IO[()]
solveShowNs xs cns  = sequence (fmap showNode (solveNs xs cns)) -- source: michael

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]


-- NOT SURE about this
-- findn out emptyN + emptyNrc             
emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0) gridConstr)

emptyNrc :: Node
emptyNrc = (\ _ -> 0,constraints (\ _ -> 0) nrcGridConstr)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

--edit                         
rsuccNode :: Node -> Constrnt -> IO [Node]
rsuccNode (s,cs) cons = do 
  xs <- getRandomCnstr cs
  if null xs 
    then return []
    else return 
      (extendNode (s,cs\\xs) cons (head xs))                          

rsolveNs :: [Node] -> Constrnt -> IO [Node] 
rsolveNs ns cons = rsearch (\x -> rsuccNode x cons) solved (return ns)

-- jaja 
rsearch :: (node -> IO [node]) 
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
       then return []
       else 
         if goal (head xs) 
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys 
                      then return [head ys]
                      else if null (tail xs) then return []
                           else 
                             rsearch 
                               succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN] gridConstr
                     return r

genRandomNrcSudoku :: IO Node
genRandomNrcSudoku = do 
  [r] <- rsolveNs [emptyNrc] nrcGridConstr
  return r                 

randomS = genRandomSudoku >>= showNode

--uniqueSol :: Node -> Bool
--uniqueSol node = singleton (solveNs [node] gridConstr) where 
--  singleton [] = False
--  singleton [x] = True
--  singleton (x:y:zs) = False

uniqueSol :: Node -> Constrnt -> Bool
uniqueSol node cns = singleton (solveNs [node] cns) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> Position -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

-- edit added constraint
eraseN :: Node -> Position -> Constrnt -> Node
eraseN n (r,c) cns = (s, constraints s cns) 
  where s = eraseS (fst n) (r,c)

-- edit added constraint 
minimalize :: Node -> [Position] -> Constrnt -> Node
minimalize n [] _ = n
minimalize n ((r,c):rcs) cns | uniqueSol n' cns = minimalize n' rcs cns
                              | otherwise    = minimalize n  rcs cns
  where n' = eraseN n (r,c) cns

filledPositions :: Sudoku -> [Position]
filledPositions s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys gridConstr)
   where xs = filledPositions (fst n)

genProblemNrc :: Node -> IO Node
genProblemNrc n = do 
  ys <- randomize xs
  return (minimalize n ys nrcGridConstr)
  where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [(emptyN)] gridConstr
          showNode r
          s  <- genProblem r
          showNode s

