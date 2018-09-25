module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{------------------------------------------------------------------------------
    Assignment X
    Time: X min

    STATE: X
------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
    Assignment 1
    Time: 30 min
These properties were tested manually with the following test data:

TODO
------------------------------------------------------------------------------}
contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\ v -> (evl v f1) -->  (evl v f2)) (allVals f1)

equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1
{------------------------------------------------------------------------------
    Assignment 2
    Time: 15 min

------------------------------------------------------------------------------}

a2 = do
  print ((parse "+(1 2)") == ([Dsj [Prop 1, Prop 2]]))
  print ((parse "*(1 2)") == ([Cnj [Prop 1, Prop 2]]))
  -- TODO

{------------------------------------------------------------------------------
    Assignment 3
    Time: 180 min

------------------------------------------------------------------------------}

dl :: Form -> Form
dl (Prop x) = Prop x
dl (Neg f) = Neg (dl f)
dl (Cnj [f1, (Dsj [f2, f3])]) = Dsj [dl (Cnj [dl f1, dl f2]), dl (Cnj [dl f1, dl f3])]
dl (Cnj [(Dsj [f2, f3]), f1]) = Dsj [dl (Cnj [dl f1, dl f2]), dl (Cnj [dl f1, dl f3])]
dl (Cnj fs) = Cnj (map dl fs)
dl (Dsj [(Cnj [f1, f2]), (Cnj [f3, f4])]) = Cnj [dl (Dsj [dl f1, dl f3]), dl (Dsj [dl f1, dl f4]), dl (Dsj [dl f2, dl f3]), dl (Dsj [dl f2, dl f4])]
dl (Dsj [f1, (Cnj [f2, f3])]) = Cnj [dl (Dsj [dl f1, dl f2]), dl (Dsj [dl f1, dl f3])]
dl (Dsj [(Cnj [f2, f3]), f1]) = Cnj [dl (Dsj [dl f1, dl f2]), dl (Dsj [dl f1, dl f3])]
dl (Dsj fs) = Dsj (map dl fs)
dl (Impl f1 f2) = Impl (dl f1) (dl f2)
dl (Equiv f1 f2) = Equiv (dl f1) (dl f2)

fl :: Form -> Form
fl (Prop x) = Prop x
fl (Neg f) = Neg (fl f)
fl (Cnj [f1, (Cnj [f2, f3])]) = Cnj [fl f1, fl f2, fl f3]
fl (Cnj [(Cnj [f2, f3]), f1]) = Cnj [fl f1, fl f2, fl f3]
fl (Cnj fs) = Cnj (map fl fs)
fl (Dsj [(Dsj [f1, f2]), (Dsj [f3, f4])]) = Dsj [fl f1, fl f2, fl f3, fl f4]
fl (Dsj [f1, (Dsj [f2, f3])]) = Dsj [fl f1, fl f2, fl f3]
fl (Dsj [(Dsj [f2, f3]), f1]) = Dsj [fl f1, fl f2, fl f3]
fl (Dsj fs) = Dsj (map fl fs)
fl (Impl f1 f2) = Impl (fl f1) (fl f2)
fl (Equiv f1 f2) = Equiv (fl f1) (fl f2)

toCnf :: Form -> Form
toCnf f = fl $ dl $ nnf $ arrowfree f

toCnf2 :: [Form] -> [Form]
toCnf2 f = map toCnf f

{------------------------------------------------------------------------------
    Assignment 4
    Time: 180 min

------------------------------------------------------------------------------}

genForm :: Int -> Form
genForm x | x < 5 = Prop x
          | x < 10 =  Neg (genForm (x-5))
          | x < 15 = Cnj [genForm (x-5), genForm (x-6)]
          | x < 20 = Dsj [genForm (x-5), genForm (x-6)]
          | x < 25 = Impl (genForm (x-5)) (genForm (x-6))
          | x < 30 = Equiv (genForm (x-5)) (genForm (x-6))
          | otherwise = (Prop x)

instance Arbitrary Form where
  arbitrary = do
    x <- arbitrary
    return (genForm x)

containsNoArrows :: Form -> Bool
containsNoArrows (Prop x) = True
containsNoArrows (Neg f) = containsNoArrows f
containsNoArrows (Cnj fs) = all (containsNoArrows) fs
containsNoArrows (Dsj fs) = all (containsNoArrows) fs
containsNoArrows (Impl f1 f2) = False
containsNoArrows (Equiv f1 f2) = False

prop_ContainsNoArrows :: Form -> Bool
prop_ContainsNoArrows f = containsNoArrows (toCnf f)

isInNnf :: Form -> Bool
isInNnf (Prop x) = True
isInNnf (Neg (Cnj fs)) = False
isInNnf (Neg (Dsj fs)) = False
isInNnf (Neg f) = isInNnf f
isInNnf (Cnj fs) = all (isInNnf) fs
isInNnf (Dsj fs) = all (isInNnf) fs
isInNnf (Impl f1 f2) = all (isInNnf) [f1, f2]
isInNnf (Equiv f1 f2) = all (isInNnf) [f1, f2]

prop_IsInNnf :: Form -> Bool
prop_IsInNnf f = isInNnf (toCnf f)

isDistributed :: Form -> Bool
isDistributed (Prop x) = True
isDistributed (Neg f) = isDistributed f
isDistributed (Cnj [f1, (Dsj fs)]) = False
isDistributed (Cnj [(Dsj fs), f1]) = False
isDistributed (Cnj fs) = all (isDistributed) fs
isDistributed (Dsj [f1, (Cnj fs)]) = False
isDistributed (Dsj [(Cnj fs), f1]) = False
isDistributed (Dsj fs) = all (isDistributed) fs
isDistributed (Impl f1 f2) = all (isDistributed) [f1, f2]
isDistributed (Equiv f1 f2) = all (isDistributed) [f1, f2]

prop_IsDistributed :: Form -> Bool
prop_IsDistributed f = isDistributed (toCnf f)
