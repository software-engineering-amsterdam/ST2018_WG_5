module Lab4 where

import Data.List
import System.Random
import Control.Monad(liftM, liftM2)
import Test.QuickCheck

{-- START OF LECTURE CODE --}
{-- Sets implemented as ordered lists without duplicates --}

newtype Set a = Set [a] deriving (Eq,Ord)

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet  :: Set a
emptySet = Set []

isEmpty  :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _        = False

inSet  :: (Ord a) => a -> Set a -> Bool
inSet x (Set s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []) _       = True
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set

insertSet :: (Ord a) => a -> Set a -> Set a
insertSet x (Set s) = Set (insertList x s)

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of
                                 GT -> y : insertList x ys'
                                 EQ -> ys
                                 _  -> x : ys

deleteSet :: Ord a => a -> Set a -> Set a
deleteSet x (Set s) = Set (deleteList x s)

deleteList x [] = []
deleteList x ys@(y:ys') = case compare x y of
                                 GT -> y : deleteList x ys'
                                 EQ -> ys'
                                 _  -> ys

list2set :: Ord a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = Set (foldr insertList [] xs)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) =
   Set (sort (map (\xs -> (list2set xs)) (powerList xs)))

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs)
                     ++ (map (x:) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a
(Set xs) !!! n = xs !! n

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  =
   insertSet x (unionSet (Set xs) set2)

{-- END OF LECTURE CODE --}

{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: ?
  Answer:
    - TODO

------------------------------------------------------------------------------}


{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 1

------------------------------------------------------------------------------}

generateRandomInts :: Int -> Int -> Int -> IO [Int]
generateRandomInts 0 x y = return []
generateRandomInts n x y = do
        p <- getStdRandom (randomR (x,y))
        ps <- generateRandomInts (n-1) x y
        return (p:ps)

insertSetList :: [Int] -> Set Int -> Set Int
insertSetList [] res = res
insertSetList (x:xs) res = insertSetList xs (insertSet x res) 

generateSetWithSize :: Int -> Int -> IO (Set Int)
generateSetWithSize n max = do
    let empty = emptySet
    rands <- generateRandomInts n 0 max
    let res = insertSetList rands empty
    return res

instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = liftM Set arbitrary

randomSetTest :: Set Int -> Bool
randomSetTest xs = isEmpty xs || (not . isEmpty) xs

assignment2Quick = quickCheck randomSetTest

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 1

------------------------------------------------------------------------------}

recurseIntersection :: Ord a => Set a -> Set a -> Set a -> Set a
recurseIntersection (Set []) b res = res 
recurseIntersection (Set (x:xs)) b res = if inSet x b 
                            then recurseIntersection (Set xs) b (insertSet x res)
                            else recurseIntersection (Set xs) b res 

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection a b = recurseIntersection a b emptySet

recurseUnion :: Ord a => Set a -> Set a -> Set a -> Set a
recurseUnion (Set []) b res = res
recurseUnion (Set (x:xs)) b res = recurseUnion (Set xs) b (insertSet x res)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion a b = recurseUnion a b emptySet

recurseDifference :: Ord a => Set a -> Set a -> Set a -> Set a
recurseDifference (Set []) b res = res
recurseDifference (Set (x:xs)) b res = if inSet x b
                            then recurseDifference (Set xs) b res
                            else recurseDifference (Set xs) b (insertSet x res)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference a b = recurseDifference a b emptySet

allSetList :: Ord a => (a -> Bool) -> Set a -> Bool
allSetList f (Set lst) = all f lst 

testIntersection :: Ord a => Set a -> Set a -> Bool
testIntersection (Set a) (Set b) = allSetList (\ x -> x `elem` a && x `elem` b) temp
    where temp = setIntersection (Set a) (Set b)

testUnion :: Ord a => Set a -> Set a -> Bool
testUnion (Set a) (Set b) = allSetList (\ x -> x `elem` a || x `elem` b) temp
    where temp = setUnion (Set a) (Set b)

testDifference :: Ord a => Set a -> Set a -> Bool
testDifference (Set a) (Set b) = allSetList (\ x -> (x `elem` a) && (x `notElem` b)) temp
    where temp = setDifference (Set a) (Set b)

testIntIntersection :: [Int] -> [Int] -> Bool
testIntIntersection a b = testIntersection (list2set a) (list2set b)
testIntUnion :: [Int] -> [Int] -> Bool
testIntUnion a b = testUnion (list2set a) (list2set b) 
testIntDifference :: [Int] -> [Int] -> Bool
testIntDifference a b = testDifference (list2set a) (list2set b)

assignment3OwnGenerator = do 
            setA <- generateSetWithSize 100 50
            setB <- generateSetWithSize 100 50
            print (testIntersection setA setB)
            print (testIntersection setB setA)
            print (testUnion setA setB)
            print (testUnion setB setA)
            print (testDifference setA setB)
            print (testDifference setB setA)

quickCheckIntersection = quickCheck testIntIntersection
quickCheckUnion = quickCheck testIntUnion
quickCheckDifference = quickCheck testIntDifference

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent:
  Answer:
    - TODO

------------------------------------------------------------------------------}



{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: 0.5

------------------------------------------------------------------------------}

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos = concatMap (\ (x,y) -> if x == y then [(x,y)] else [(x,y), (y, x)])

testSymClos = symClos [(1,2), (2,3), (3,4)]
testSymClosDouble = symClos [(1,1), (2,3), (4,3), (6,4)]

{------------------------------------------------------------------------------

  Assignment 6

  Hours spent: ?
  Answer:

------------------------------------------------------------------------------}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

recurseTrClos :: Ord a => Rel a -> Rel a -> Rel a
recurseTrClos [] full = []
recurseTrClos x full = temp++(recurseTrClos temp full) where temp = x @@ full

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/=x) xs)

trClos :: Ord a => Rel a -> Rel a
trClos a = removeDuplicates (concatMap (\x -> x : recurseTrClos [x] a) a)

testTrClos = trClos [(1,2),(2,3),(3,4)]
testTrClos2 = trClos [(1,2),(1,4),(2,3),(3,4)]

{------------------------------------------------------------------------------

  Assignment 7

  Hours spent: ?
 
------------------------------------------------------------------------------}


{------------------------------------------------------------------------------

  Assignment 8

  Hours spent: ?

------------------------------------------------------------------------------}

