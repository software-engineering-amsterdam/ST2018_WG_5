module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Debug.Trace
import Lecture3

{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: 1

------------------------------------------------------------------------------}
-- Should always be false and thus not satisfiable
contradiction :: Form -> Bool
contradiction x = not (any (\ v -> evl v x) (allVals x))

-- Should always be true
tautology :: Form -> Bool
tautology x = all (\ v -> evl v x) (allVals x)

-- If the first statement is true the second one should also be true
-- So in this implementation I take all possible false y valuations and check if
-- the valuation would also be false in x. If not (thus true) this means true -> false
-- which is of course no entailment
entails :: Form -> Form -> Bool
entails x y = all (\r -> not (evl r x)) falseYVals
  where
    yVals = allVals y
    falseYVals = filter (\r -> not (evl r y)) yVals

-- 2 statements are equivalent when all possible valuations have the same result in
-- both statements.
equiv :: Form -> Form -> Bool
equiv x y = all (==True) (map (\r -> (evl r x) == (evl r y)) xyVals)
  where
    xyVals = (allVals x) ++ (allVals y)

assignment1 = do
    -- p∧(¬p)
    print (contradiction (Cnj [p, (Neg p)]))
    -- p∧(q)
    print (contradiction (Cnj [p, q]))
    -- p∨(¬p)
    print (tautology (Dsj [p, (Neg p)]))
    -- p^q
    print (tautology (Cnj [p, q]))
    -- p |= p ∨ q
    print (entails (p) (Dsj [p, q]))
    -- p |= p ∧ q
    print (entails (p) (Cnj [p, q]))
    -- (￢p ∧ ￢q) === ￢(p ∨ q)
    print (equiv (Cnj [(Neg p), (Neg q) ]) (Neg (Dsj [q, p])))
    -- (￢p ∧ ￢q) != (p ∨ q)
    print (equiv (Cnj [(Neg p), (Neg q) ]) ((Dsj [q, p])))
-- Output:
-- True
-- False
-- True
-- False
-- True
-- False
-- True
-- False

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 3
  Answer:
    - I created a generator which creates various sorts of formulas. After converting these to
      a String, I parse them and check if they are the same as the original
    - For the generator I got some inspiration from:
        https://stackoverflow.com/questions/35726256/quickcheck-on-custom-datatype

------------------------------------------------------------------------------}
-- Generator by implementing every data type as Arbitrary
instance Arbitrary Form where
    arbitrary = oneof [arbitraryProp, arbitraryNeg, arbitraryCnj, arbitraryDsj, arbitraryImpl,arbitraryEquiv]
        where arbitraryProp = do
                  p <- arbitrary
                  return $ Prop $ ((p `mod` 3) + 1)
              arbitraryNeg = do
                  p <- arbitrary
                  return $ Neg $ p
              arbitraryCnj = do
                  p <- arbitrary
                  return $ Cnj [p,p]
              arbitraryDsj = do
                  p <- arbitrary
                  return $ Dsj [p,p]
              arbitraryImpl = do
                  p <- arbitrary
                  return $ Impl p p
              arbitraryEquiv = do
                  p <- arbitrary
                  return $ Equiv p p

-- Tests if the form as String and then parsed is the same as the original
testParse :: Form -> Bool
testParse x = x == head (parse ((show x)))

-- The quickCheck test. It runs quite slow and gets a little stuck a few times
-- this is due the big chance of creating very large Forms due to nestings
assignment2 = quickCheckWith stdArgs {maxSize = 3, maxSuccess = 50} testParse
-- Output: +++ OK, passed 50 tests.

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 0.5
  Answer:
    - I've used the 2 functions in the lecture notes to perform the 2 necessary steps
      for converting to CNF
    - Also added  a few test cases

------------------------------------------------------------------------------}

assignment3 :: Form -> Form
assignment3 a =  nnf (arrowfree a)

assignment3Tests = do
  -- (p → q)   ↔   (¬p ∨ q)
  print (Impl p q)
  print (assignment3 (Impl p q))
  -- (p ↔ q)   ↔   (¬p∨q)∧(p∨¬q)
  print (Equiv p q)
  print (assignment3 (Equiv p q))
  -- ¬(p⋀q)   ↔   (¬p)⋀(¬q)
  print (Neg (Dsj [p,q]))
  print (assignment3 (Neg (Dsj [p,q])))
-- Output:
-- (1==>2)
-- +(-1 2)
-- (1<=>2)
-- +(*(1 2) *(-1 -2))
-- -+(1 2)
-- *(-1 -2)

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 1
  Answer:
    - Related properties are:
        - The in and output must be equivalent to eachother
        - Does not contain <-> or ->
        - Every Neg can only have a Prop as child

------------------------------------------------------------------------------}

-- Checks for the first step which removes arrows
containsNoArrows :: Form -> Bool
containsNoArrows x = x == (arrowfree x)

-- Checks if truth tables are still the same
sameEquivalence :: Form -> Form -> Bool
sameEquivalence x y = equiv x y

-- Checks if there's no Neg on  Cnj, Dsj, Equiv or Impl
atomsOnlyNegated :: Form -> Bool
atomsOnlyNegated (Prop _) = True
atomsOnlyNegated (Neg (Cnj _)) = False
atomsOnlyNegated (Neg (Dsj _)) = False
atomsOnlyNegated (Neg (Equiv _ _)) = False
atomsOnlyNegated (Neg (Impl _ _)) = False
atomsOnlyNegated (Cnj x) = all atomsOnlyNegated x
atomsOnlyNegated (Dsj x) = all atomsOnlyNegated x
atomsOnlyNegated (Equiv x y) = atomsOnlyNegated x && atomsOnlyNegated y
atomsOnlyNegated (Impl x y) = atomsOnlyNegated x && atomsOnlyNegated y
atomsOnlyNegated (Neg x) = atomsOnlyNegated x

testCNF :: Form -> Bool
testCNF x =  containsNoArrows result  && sameEquivalence x result && atomsOnlyNegated result
  where
    result = assignment3 x

-- Same kind of usage of quickCheck as assignment 2
assignment4 = quickCheckWith stdArgs {maxSize = 3, maxSuccess = 50} testCNF
-- Output: +++ OK, passed 50 tests.
