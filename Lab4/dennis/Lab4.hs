module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: 2
  Answer:

    - The Russel Paradox is not entirely clear to me.
    - The halts function is not very clear to me.

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 2

------------------------------------------------------------------------------}

-- NOT WORKING
--getRandomInt :: IO Int
--getRandomInt = randomRIO (minBound :: Int, maxBound :: Int) :: IO Int
--
--getRandomIntList :: IO Int -> IO [IO Int]
--getRandomIntList 0 = return []
--getRandomIntList n = do
--                      return (getRandomInt:(getRandomIntList (n-1)))
--
--ass2 = do
--        x <- getRandomIntList 100 :: IO [IO Int]
--        print x


-- COPIED FROM MICHAEL FOR REST OF ASSIGNMENT
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


instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = fmap Set arbitrary

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

-- This property fails, because quickcheck does not generate unique values.
prop_ContainsNoDoubleElements :: Eq a => Set a -> Bool
prop_ContainsNoDoubleElements (Set xs) = all (\x -> (count x xs) == 1) xs
-- *** Failed! Falsifiable (after 4 tests):
-- {(),()}

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 1.25

------------------------------------------------------------------------------}

s2l :: Set a -> [a]
s2l (Set a) = a

intersection' :: Eq a => Set a -> Set a -> Set a
intersection' (Set a) (Set b) = Set (intersect a b)

union' :: Eq a => Set a -> Set a -> Set a
union' (Set a) (Set b) = Set (union a b)

difference' :: Eq a => Set a -> Set a -> Set a
difference' (Set a) (Set b) = Set (union (a \\ b) (b \\ a))

testIntersection :: Eq a => Set a -> Set a -> Bool
testIntersection (Set a) (Set b) = all (\x -> x `elem` a && x `elem` b) x
                          where x = s2l (intersection' (Set a) (Set b))

testUnion :: Eq a => Set a -> Set a -> Bool
testUnion (Set a) (Set b) = all (\x -> x `elem` a || x `elem` b) x
                          where x = s2l (union' (Set a) (Set b))

testDifference :: Eq a => Set a -> Set a -> Bool
testDifference (Set a) (Set b) = all (\x -> (x `elem` a) /= (x `elem` b)) x
                          where x = s2l (difference' (Set a) (Set b))

ass3 = do
        a <- genRandSet
        b <- genRandSet
        print (testIntersection a b)
        print (testUnion a b)
        print (testDifference a b)
-- OUTPUT:
-- True
-- True
-- True


-- *Lab4> quickCheck testDifference
-- *** Failed! Falsifiable (after 4 tests):
-- {(),()}
-- {()}
-- *Lab4> quickCheck testUnion
-- +++ OK, passed 100 tests.
-- *Lab4> quickCheck testIntersection
-- +++ OK, passed 100 tests.

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 2
  Answer:

    -

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

tr :: Eq a => Rel a -> Rel a -> Rel a
tr x xs = x ++ (x @@ xs) ++ if y /= [] then tr y xs else [] where y = (x @@ xs)

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos (x:xs) = nub $ (tr [x] xs) ++ trClos xs

-- *Lab4> trClos [(1,2),(2,3),(3,4)]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

{------------------------------------------------------------------------------

  Assignment 7

  Hours spent: 2
  Answer:

------------------------------------------------------------------------------}

newtype RelInt = RelInt (Rel Int)
  deriving (Eq, Show)

instance Arbitrary RelInt where
   arbitrary = do
     Positive x <- arbitrary
     Positive y <- arbitrary
     return $ (RelInt [(x,y)])

prop_ContainsSymClose :: RelInt -> Bool
prop_ContainsSymClose (RelInt r) = all (\(x,y) -> (y,x) `elem` (symClos r)) r

--prop_ContainsTransitive :: RelInt -> Bool
--prop_ContainsTransitive (RelInt r) =  x
--                                    where x = trClose r

{------------------------------------------------------------------------------

  Assignment 8

  Hours spent:
  Answer:
    -
------------------------------------------------------------------------------}

