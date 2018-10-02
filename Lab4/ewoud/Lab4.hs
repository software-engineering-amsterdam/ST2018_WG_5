module Lab4 where

import System.Random
import Data.List
import Data.Tuple
import Test.QuickCheck
import Control.Monad(liftM, liftM2)
import Data.Function (on)
import SetOrd


{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: 2h

------------------------------------------------------------------------------}
-- Page 136
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

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 0.5h

------------------------------------------------------------------------------}
-- Version from scratch:

-- generates a random Ints.
randomInt :: IO Int
randomInt = randomRIO (0, 100)

-- generates a random list of Ints
-- Source: https://wiki.haskell.org/Examples/Random_list (I got this source from Michael).
randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

-- Returns a random list of Ints.
genRandList :: IO [Int]
genRandList = do
    seed  <- newStdGen
    n <- randomInt 
    let rs = randomList n seed
    return rs

-- Converts a random lists of Ints to a random set of Ints.
genRandSet :: IO (Set Int)
genRandSet = liftM list2set genRandList

-- Quickcheck version:

-- Make the Set structure part quichckeck.
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = liftM Set arbitrary

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 

  NOTES: perhaps the trick is to use unionSet in SetOrd.hs where we are expected to
  rewrite the set operations using unionSet as the basis.
  STATE: MISSING properties?
------------------------------------------------------------------------------}
--set intersection
intersectSet :: (Ord a) => Set a -> Set a -> Set a 
intersectSet (Set xs) (Set ys) = list2set $ xs `intersect` ys

--set union
unionSet' :: (Ord a) => Set a -> Set a -> Set a 
unionSet' (Set xs) (Set ys) = list2set $ xs `union` ys

--set difference
differenceSet :: (Ord a) => Set a -> Set a -> Set a 
differenceSet (Set xs) (Set ys) = list2set $ xs \\ ys

--
-- Testing properties
--

-- Helper functions:

-- AllSet idea gestolen van Michael.
allSet :: (a -> Bool) -> Set a -> Bool
allSet predic (Set xs) = all predic xs

-- Turns a Set into a list
set2List :: Set Int -> [Int]
set2List (Set []) = []
set2List (Set xs) = xs

-- intersection test
-- Check if all elements of set A and B are in the Union set 
prop_elementsIntersect :: Set Int -> Set Int -> Bool
prop_elementsIntersect (Set xs) (Set ys) = allSet (\x -> (elem x xs) && (elem x ys)) setU
    where 
        setU = intersectSet (Set xs) (Set ys)

-- TODO: check elements from intersect set iff they are in set A and set B

-- union test
-- Check if all elements of set A or B are in the Union set
prop_elementsInUnion :: Set Int -> Set Int -> Bool
prop_elementsInUnion (Set xs) (Set ys) = allSet (\u -> (elem u xs) || (elem u ys)) setU
    where 
        setU = unionSet' (Set xs) (Set ys)

-- Check if the Union set only contains elements from set A or set B
-- TODO: Checke me?
prop_unionFromElem :: Set Int -> Set Int -> Bool
prop_unionFromElem (Set xs) (Set ys) = allSet (\u -> (elem u setU) ) (Set xs) || allSet (\u -> (elem u setU) ) (Set ys)
    where 
        setU = set2List $ unionSet' (Set xs) (Set ys) 

-- difference test
-- (A U B ) - (A - B) == B
-- Check if the 

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 2h

------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: 0.25h

------------------------------------------------------------------------------}
type Rel a = [(a,a)]

-- Sorts tuple pairs based on the first and then the second element.
sortedPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortedPairs xs = sortOn fst $ sortOn snd xs

-- The symmetric closure of R is the union of R with its converse relation, RT (aka R swapped).
-- Source: wikipedia
symClos :: Ord a => Rel a -> Rel a
symClos r = sortedPairs $ union r $ map swap r


{------------------------------------------------------------------------------

  Assignment 6

  Hours spent: 0.5h

------------------------------------------------------------------------------}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- maps the fst element of each tuple up to the equal snd element.
tupleRange :: Eq a => (a -> a) -> a -> a
tupleRange x y 
    | y == x y = y
    | otherwise = tupleRange x (x y)

trClos :: (Ord a, Eq a) => Rel a -> Rel a
trClos x = tupleRange closure x 
    where closure x = sortedPairs $ union x (x @@ x)

{------------------------------------------------------------------------------

  Assignment 7

  Hours spent: 1.5h
  STATE: not sure if i hall all properties for the tests
------------------------------------------------------------------------------}

isSubSet :: Eq a => Rel a -> Rel a -> Bool
isSubSet xs ys =  all (`elem` ys) xs

--properties symmetric closure:
isSymmetricSet :: Ord a => Rel a -> Bool
isSymmetricSet xs = all (\(x , y) -> (y , x) `elem` xs) xs

prop_symClos :: Ord a => Rel a -> Bool
prop_symClos setA = isSubSet setA setB && isSymmetricSet setB
    where setB = symClos setA

-- properties transitive closure
isTransitiveSet :: Eq a => Rel a -> Bool
isTransitiveSet xs = all (`elem` xs) (xs @@ xs)

prop_trClos :: Ord a => Rel a -> Bool
prop_trClos setA = isSubSet setA setB && isTransitiveSet setB
    where setB = trClos setA
{------------------------------------------------------------------------------

  Assignment 8

  Hours spent: 
  TODO: FIX Couldn't match type ‘IO (Rel Int)’ with ‘[(a0, a0)]’
  waarschijnlijk super simpel maar het is laat en ik moet nu echt slapen
------------------------------------------------------------------------------}

-- Generates listed tuples in the form of [(x,y)] as [(22,34)]
testCases  :: IO (Rel Int)
testCases = do
    x <- randomInt
    y <- randomInt  
    return ([(x, y)])

testfin = do
    relA <- testCases
    let transSymRelA = trClos (symClos relA)
    let symTransRelA = symClos (trClos relA)
    print relA
    print $ transSymRelA == symTransRelA

-- FIXME?
-- wanna use testCases in my prop
prop_counterExample :: Ord a => Rel a -> Bool
--prop_counterExample (xs ) = False
prop_counterExample xs = (trClos (symClos xs)) == (symClos (trClos xs))
--prop_counterExample = (trClos (symClos testCases)) == (symClos (trClos testCases))

--qctest = quickCheck prop_counterExample

--qctest2 = quickCheck (\ (Positive a) (Positive b) -> trClos [(a,a)] == trClos [(a,a)])