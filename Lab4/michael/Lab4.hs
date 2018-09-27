module Lab4 where

import Data.List
import System.Random
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

  Hours spent: 1.25

------------------------------------------------------------------------------}
setInter :: Eq a => Set a -> Set a -> Set a
setInter (Set x) (Set y) = Set (x `intersect` y)

setUnion :: Eq a => Set a -> Set a -> Set a
setUnion (Set x) (Set y) = Set (x `union` y)

setDiff :: Eq a => Set a -> Set a -> Set a
setDiff (Set x) (Set y) = Set (x \\ y)

-- Utility function for performing the haskell "all" function on a Set
allSet :: (a -> Bool) -> Set a -> Bool
allSet predic (Set xs) = all predic xs

-- Test if the result is contained in the first and the second list
setInterTest :: Set Int -> Set Int -> Bool
setInterTest (Set xs) (Set ys) = allSet (\x -> (elem x xs) && (elem x ys)) result
            where
                 result = setInter (Set xs) (Set ys)

-- Test if the result is contained in the first or the second list
setUnionTest :: Set Int -> Set Int -> Bool
setUnionTest (Set xs) (Set ys) = allSet (\x -> (elem x xs) || (elem x ys)) result
            where
                 result = setUnion (Set xs) (Set ys)

-- Test if the result is contained in the first list, but not in the second
setDiffTest :: Set Int -> Set Int -> Bool
setDiffTest (Set xs) (Set ys) = allSet (\x -> (elem x xs) || not (elem x ys)) result
            where
                 result = setDiff (Set xs) (Set ys)

assignment3SelfMade = do
            x <- genRandSet
            y <- genRandSet
            print "Intersection test"
            print (setInterTest x y)
            print (setInterTest y x)
            print "Union test"
            print (setUnionTest x y)
            print (setUnionTest y x)
            print "Difference test"
            print (setDiffTest x y)
            print (setDiffTest y x)
{-
    Output:
        True
        True
        "Union test"
        True
        True
        "Difference test"
        True
        True
-}

assignment3QuickCheck = do
                print "Intersection test"
                quickCheck setInterTest
                print "Union test"
                quickCheck setUnionTest
                print "Difference test"
                quickCheck setDiffTest
{-
    Output:
        "Intersection test"
        +++ OK, passed 100 tests.
        "Union test"
        +++ OK, passed 100 tests.
        "Difference test"
        +++ OK, passed 100 tests.
-}

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

-- Also need to take into account that a pair with 2 of the same should not occur twice afterwards
symClos :: Ord a => Rel a -> Rel a
symClos xs = concat (map (\(a,b) -> if a == b then [(a,b)] else [(a,b), (b,a)]) xs)

assignment5Test1 = symClos [(1,2),(2,3),(3,4)]
-- Output: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
assignment5Test2 = symClos [(1,2),(3,3),(3,4)]
-- Output: [(1,2),(2,1),(3,3),(3,4),(4,3)]

{------------------------------------------------------------------------------

  Assignment 6

  Hours spent: 1
  Answer:
    So my approach is working as follows:
    By taking the original Set of binary relations and applying the composition function to itself
    (@@) this will create a set that should be part of the transitive closure. Adding these though
    will require some additional relations before it's transitive. So we perform this method over and over
    (stripping duplicates in the mean time) until there is nothing being added, which means we got to
    the transitive closure.

------------------------------------------------------------------------------}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- This take the relations and adds the result of the composition with itself and strips duplicates afterwards
addTransitives :: Eq a => Rel a -> Rel a
addTransitives xs = rmDuplicates (xs ++ (xs @@ xs))

-- removes duplicates from a list
rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates [] = []
rmDuplicates (x:xs) = x : rmDuplicates (filter (/=x) xs)

-- Again. the stop condition is when nothing new is added after performing "addTransitives"
trClos :: (Ord a, Eq a) => Rel a -> Rel a
trClos xs = if result == xs then xs else trClos result
        where
            result = addTransitives xs

assignment6Test1 = trClos [(1,2),(2,3),(3,4)]
-- Output: [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]
assignment6Test2 = trClos [(1,2),(2,3),(3,4),(4,5)]
-- Output: [(1,2),(2,3),(3,4),(4,5),(1,3),(2,4),(3,5),(1,4),(2,5),(1,5)]

{------------------------------------------------------------------------------

  Assignment 7

  Hours spent: 1
  Answer:
    -   For the symClose. The property that must be checked is whether for every pair (x,y) there is a pair
        (y,x) in the set
    -   For trClose. The property that must be checked is whether for every pair (x,y) and a pair (y,z) in the
        the set there is also a (x,z) in the set.
    -   For both the original set MUST be a subset of the newly created set (we can use subSet for this)

------------------------------------------------------------------------------}
-- Checks if every relation in the set is also there in a swapped version
isSym :: Eq a => Rel a -> Bool
isSym xs = and (map (\x -> elem (swap x) xs) xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- Checks if every (x,y) and (y,z) lead to a (x,z) relation in the set as well
isTrans :: Eq a => Rel a -> Bool
isTrans xs = and [elem(a,d) xs | (a,b) <- xs, (c,d) <- xs, b == c]

{------------------------------------------------------------------------------

  Assignment 8

  Hours spent: 

------------------------------------------------------------------------------}

