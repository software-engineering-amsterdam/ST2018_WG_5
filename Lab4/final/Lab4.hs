module Lab4 where

import System.Random
import Test.QuickCheck
import System.Random
import Data.List
import Data.Tuple
import Test.QuickCheck
import Control.Monad(liftM, liftM2)
import Data.Function (on)

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

  Answer:
    - The Russel Paradox is not entirely clear to me.
    - The halts function is not very clear to me.
    - Page 136
    - Notation in example 4.6 doesn’t really make the point clear for me
    - Exercise 4.26 and 4.27. Coming up with proof (proof in general)
    - What is a real world use case of a complement?
    - Are there other ways to represent sets in Haskell which do not use lists?

    -- Remark. The question whether the equation a = { a } has solutions (or, more
    -- generally,whether sets a exist such that a ∈ a) is answered differentlyby different
    -- axiomatizations of set theory.
    --
    -- What does this mean?

    -- Page 137
    -- Exercise 4.11 Explain that ∅ != {∅}
    -- Is that because {∅} is not empty because it has an empty element?

    -- Page 149
    -- Exercise 4.40 1. Assume that A and B are non-empty and that A × B = B × A. Show that A = B.
    -- 2. Show by means of an example that the condition of non-emptiness in 1 is necessary. (Did you use this in your proof of 1?)
    --
    -- I think i can solve this problem using a very simple case where A = B = {1,2}
    -- But how can I verify that my simple example is enough to proof that non-emptiness in 1 is necessary

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 1.5

------------------------------------------------------------------------------}
-- Generate a random set size. (max 1000)
genRandSetSize :: IO Int
genRandSetSize = randomRIO (0, 1000)

-- Used this source for generating a random set of Int: https://wiki.haskell.org/Examples/Random_list
-- Modified it afterwards to also have a random size + work with the Set data type
randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

genRandSet :: IO (Set Int)
genRandSet = do
    seed  <- newStdGen
    length <- genRandSetSize
    let rs = randomlist length seed
    return  (Set rs)
-- I.E. Output:
-- {1741263264999336083,-4200649958150071639,3205523063621367757,-8725295676701910249,8153416671447191634,3025777913354096002,4366097170603585049,....}


-- With some inspiration of https://stackoverflow.com/questions/40089769/making-an-arbitrary-instance-for-a-newtype-that-uses-maybe
-- This generates random sets containing random entries
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = fmap Set arbitrary

-- This is just a mock test and should always be true. It's more to showcase that it's possible to perform tests with this
randomSetListTest :: Set Int -> Bool
randomSetListTest (Set xs) = xs == (reverse (reverse xs))

quickCheckRandomList = quickCheck randomSetListTest
-- Output: +++ OK, passed 100 tests.

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
            setA <- genRandSet
            setB <- genRandSet
            print (testIntersection setA setB)
            print (testIntersection setB setA)
            print (testUnion setA setB)
            print (testUnion setB setA)
            print (testDifference setA setB)
            print (testDifference setB setA)
{-- Output:
True
True
True
True
True
True
-}

quickCheckIntersection = quickCheck testIntIntersection
-- Output: +++ OK, passed 100 tests.
quickCheckUnion = quickCheck testIntUnion
-- Output: +++ OK, passed 100 tests.
quickCheckDifference = quickCheck testIntDifference
-- Output: +++ OK, passed 100 tests.

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent:
  Answer:
    - Why is R = {(1,4),(1,5),(2,5)} is a relation from {1,2,3} to {4,5,6}
      and not a relation from {1,2} to {4,5}?
    - What is the function of the identity relation?
    - Why are the functions curry and uncurry fundamental for functional
      programming?
    - Again coming up with proof in general
    - 5.63,5.64,6.65 is not clear for me
    - Partitions. And especially such notation used at 5.83

    -- Page 200
    -- Example 5.63 The relation ∼ between vectors in 3-dimensional space R 3 that is
    -- defined by~ a ∼ ~ b ≡ ∃r ∈ R + (~ a = r ~ b) is an equivalence.
    --
    -- What does the ∼ symbol mean between the two vectors? Never seen it before in this book

    -- Page 213
    -- Integer Partitions
    -- I cannot figure out for what kind of problems integer partitions can be usefull (ignoring the cash change examples)

    -- Page 204
    -- Equivalence classes
    -- I dont see why this needs to be a new class.

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: 0.5

------------------------------------------------------------------------------}
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):xs) = ((x,y):(y,x):symClos xs)

-- *Lab4> symClos [(1,2),(2,3),(3,4)]
-- [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]

{------------------------------------------------------------------------------

  Assignment 6

  Hours spent: 0.5
  Answer:

------------------------------------------------------------------------------}
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Sorts tuple pairs based on the first and then the second element.
sortedPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortedPairs xs = sortOn fst $ sortOn snd xs

-- maps the fst element of each tuple up to the equal snd element.
tupleRange :: Eq a => (a -> a) -> a -> a
tupleRange x y
    | y == x y = y
    | otherwise = tupleRange x (x y)

trClos :: (Ord a, Eq a) => Rel a -> Rel a
trClos x = tupleRange closure x
    where closure x = sortedPairs $ union x (x @@ x)

assignment6Test1 = trClos [(1,2),(2,3),(3,4)]
-- Output: [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
assignment6Test2 = trClos [(1,2),(2,3),(3,4),(4,5)]
-- Output: [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]

{------------------------------------------------------------------------------

  Assignment 7

  Hours spent: 2

------------------------------------------------------------------------------}
isSubset :: Eq a => Rel a -> Rel a -> Bool
isSubset xs ys = all (\ x -> x `elem` ys) xs

isSwappedSubset :: Eq a => Rel a -> Rel a -> Bool
isSwappedSubset xs zs = all (\ (x,y) -> (x,y) `elem` zs && (y,x) `elem` zs ) xs

testSymmetry :: Rel Int -> Bool
testSymmetry a = isSubset a sym && isSwappedSubset a sym
    where sym = symClos a

isTransitive :: Eq a => Rel a -> Bool
isTransitive xs = and [ elem(a,c) xs | (a,b) <- xs, (c,d) <- xs, b == c ]

testTransitive :: Rel Int -> Bool
testTransitive a = isSubset a trans && isTransitive trans
    where trans = trClos a

quickCheckSymmetry = quickCheck testSymmetry
-- Output: +++ OK, passed 100 tests.
quickCheckTransitivity = quickCheck testTransitive
-- Output: +++ OK, passed 100 tests.

{------------------------------------------------------------------------------

  Assignment 8

  Hours spent: 0.25
  Answer:
    This is not true, the order of applying the 2 operations DOES matter. The main point why it does is
    that transitivity is also true whenever 2 pairs have nothing to do with each other. When symmetry is
    added first this causes the situation to change completely.
    Example:
    sym -> trans
    {(1,3),(2,4)} -> {(1,3),(3,1),(2,4),(4,2)} -> {(1,3),(3,1),(2,4),(4,2),(1,1),(3,3),(2,2),(4,4)}

    trans -> sym
    {(1,3),(2,4)} -> {(1,3),(2,4)} -> {(1,3),(3,1),(2,4),(4,2)}

    As you can see the result is different when the order of applying the operations is swapped.

------------------------------------------------------------------------------}