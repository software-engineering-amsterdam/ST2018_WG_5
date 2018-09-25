module Lab3 where

import Data.List
import Test.QuickCheck
import Control.Monad
import Lecture3

{------------------------------------------------------------------------------
    Assignment 1
    Time: 60 min

    STATE: DONE.
    Perhaps make the testing a bit more verbose / non spaghetti.

------------------------------------------------------------------------------}

-- A contradiction is a formula which is FALSE under all interpretations.
-- So p is a contradiction if, and only if, p is not satisfiable
-- EX: (p) AND not(p) = False
contradiction :: Form -> Bool
contradiction p = not $ satisfiable p

-- A tautology is a formula that is TRUE under all interpretation.
-- So f is a tautology if, and only if, not(f) is not satisfiable
-- EX: (f) OR not(f) = True
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Logical entailment means that a statement entails another statement if,
-- whenever the first is true, the second must also be true
-- So p entails q if, whenever p is true, q must also be true
-- EX: p entails (p OR q)
entails :: Form -> Form -> Bool
-- q is a tautological entailment of p if for all outcomes for the two statemens there is no
-- outcome where p is true and q is false.
-- NOTE: RESERACH: Not all logical consequences are tautological consequences (what does this mean here???, source wikipedia)
-- entails p q = tautology (Impl p q) -- contradiction (Cnj [f1, Neg f2])
entails p q = contradiction (Cnj [p, Neg q])

-- Logical equivalence means that 2 statements share the same truth value in every model.
-- So every truth assignment that satisfies p satisfies q and vice versa.
-- EX: not(p OR q) is logical equialent to (not(p) AND not(q))
equiv :: Form -> Form -> Bool
-- p and q are logically equivalent if p↔q is a tautology.
equiv p q = tautology (Equiv p q)

-- Source used for many basic proofs: http://www1.spms.ntu.edu.sg/~frederique/dm2.pdf
assignment1 = do
    -- contradiction
    -- p∧(¬p) -> True
    print (contradiction (Cnj [p, (Neg p)]))
    -- p∧(q) -> False
    print (contradiction (Cnj [p, q]))
    -- tautology
    -- p ∨ (¬p) -> True
    print (tautology (Dsj [p, (Neg p)]))
    -- P & (¬P) -> False (contradiction cannot be a tautology)
    print (tautology (Cnj [p, (Neg p)]))
    -- check entails
    -- p |= p ∨ q -> True
    print (entails (p) (Dsj [p, q]))
    -- p |= p ∧ q -> False
    print (entails (p) (Cnj [p, q]))
    -- check equiv
    -- (￢p ∧ ￢q) === ￢(p ∨ q) -> True
    print (equiv (Cnj [(Neg p), (Neg q) ]) (Neg (Dsj [q, p])))
    -- (￢p ∧ ￢q) != (p ∨ q) -> False
    print (equiv (Cnj [(Neg p), (Neg q) ]) ((Dsj [q, p])))
{------------------------------------------------------------------------------
    Assignment 2
    Time: 60 min

    STATE: DONE.
  Answer:
    - We created a generator which creates various sorts of formulas. After converting these to
      a String, we parse them and check if they are the same as the original
    - For the generator we got some inspiration from:
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
    Time: 180 min

    STATE: KIND OF DONE.
    We could not manage to completely pass the distributivity tests described in assignment 4.
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

    STATE: DONE.

------------------------------------------------------------------------------}

cnjLiftWrapper :: Form -> Form -> Form
cnjLiftWrapper x y = Cnj [x, y]

dsjLiftWrapper :: Form -> Form -> Form
dsjLiftWrapper x y = Dsj [x, y]

boundedForm :: Gen Form
boundedForm = sized form where
    form :: Int -> Gen Form
    form n
        | n > 0 = oneof [return (Prop n),
                    liftM Neg (form (n-1)),
                    liftM2 Impl (form (n `div` 2)) (form (n `div` 2)),
                    liftM2 Equiv (form (n `div` 2)) (form (n `div` 2)),
                    liftM2 cnjLiftWrapper (form (n `div` 2)) (form (n `div` 2)),
                    liftM2 dsjLiftWrapper (form (n `div` 2)) (form (n `div` 2))
                   ]
        | otherwise = return (Prop n)

--
containsNoArrows :: Form -> Bool
containsNoArrows (Prop x) = True
containsNoArrows (Neg f) =  containsNoArrows f
containsNoArrows (Cnj fs) = all containsNoArrows fs
containsNoArrows (Dsj fs) = all containsNoArrows fs
containsNoArrows (Impl f1 f2) = False
containsNoArrows (Equiv f1 f2) = False

noNegativeClauses :: Form -> Bool
noNegativeClauses (Prop x) = True
noNegativeClauses (Cnj fs) = all noNegativeClauses fs
noNegativeClauses (Dsj fs) = all noNegativeClauses fs
noNegativeClauses (Impl f1 f2) = noNegativeClauses f1 && noNegativeClauses f2
noNegativeClauses (Equiv f1 f2) = noNegativeClauses f1 && noNegativeClauses f2
noNegativeClauses (Neg (Cnj _)) = False
noNegativeClauses (Neg (Dsj _)) = False
noNegativeClauses (Neg (Impl _ _)) = False
noNegativeClauses (Neg (Equiv _ _)) = False
noNegativeClauses (Neg f) =  noNegativeClauses f

areEquivalent :: Form -> Form -> Bool
areEquivalent x y = equiv x y

noNestedAndOr :: Form -> Bool
noNestedAndOr (Prop x) = True
noNestedAndOr (Neg f) =  noNestedAndOr f
noNestedAndOr (Cnj [_, (Cnj _)]) = False
noNestedAndOr (Cnj [(Cnj _), _]) = False
noNestedAndOr (Cnj fs) = all noNestedAndOr fs
noNestedAndOr (Dsj [_, (Cnj _)]) = False
noNestedAndOr (Dsj [(Cnj _), _]) = False
noNestedAndOr (Dsj fs) = all noNestedAndOr fs
noNestedAndOr (Impl f1 f2) = noNestedAndOr f1 && noNestedAndOr f2
noNestedAndOr (Equiv f1 f2) = noNestedAndOr f1 && noNestedAndOr f2

checkIsCNF :: Form -> Bool
checkIsCNF x = containsNoArrows result && noNegativeClauses result
        && areEquivalent result x
        && noNestedAndOr result
    where result = toCnf x

-- testnow = verboseCheckWith stdArgs {maxSize = 20} (forAll boundedForm satisfiable)
assignment4 = quickCheckWith stdArgs {maxSize = 20, maxSuccess = 1000} (forAll boundedForm checkIsCNF)