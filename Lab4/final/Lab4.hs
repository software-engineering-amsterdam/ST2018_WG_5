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

  Hours spent: TODO
  Answer:
    - TODO

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: TODO

------------------------------------------------------------------------------}

--TODO

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: TODO

------------------------------------------------------------------------------}

--TODO

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent:
  Answer:
    - TODO

------------------------------------------------------------------------------}

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: TODO

------------------------------------------------------------------------------}

--TODO

{------------------------------------------------------------------------------

  Assignment 6

  Hours spent: TODO
  Answer:

------------------------------------------------------------------------------}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--TODO

{------------------------------------------------------------------------------

  Assignment 7

  Hours spent: TODO
  Answer:
    -   TODO

------------------------------------------------------------------------------}

--TODO

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
