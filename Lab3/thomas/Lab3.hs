module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad(liftM, liftM2)
import Lecture3
                           
-- Assignment 1
-- Time: 1 hour programming
-- Time testing: ?
contradiction :: Form -> Bool
contradiction = not . satisfiable

contratest = Equiv p (Neg p)

tautology :: Form -> Bool
tautology x = all (\ y -> evl y x) (allVals x) 

tautologyTest1 = Equiv p p
tautologyTest2 = Dsj [p, Neg p]

-- | logical entailment
entails :: Form -> Form -> Bool
entails a b = all (\ z -> evl z a) (filter (\ y -> evl y b) (allVals b))

-- | logical equivalence 
equiv :: Form -> Form -> Bool
equiv a b = all (\ x -> evl x a == evl x b) (allVals b) && all (\ x -> evl x a == evl x b) (allVals a)

-- Assignment 2


-- Assignment 3

cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Neg (Neg f)) = cnf f
cnf (Cnj [f1, (Dsj [f2, f3])]) = Dsj [(Cnj [cnf f1, cnf f2]) , (Cnj [cnf f1, cnf f3])]
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj [f1, (Cnj [f2, f3])]) = Cnj [(Dsj [cnf f1, cnf f2]), (Dsj [cnf f1, cnf f3])]
cnf (Dsj fs) = Dsj (map cnf fs)
cnf (Neg (Cnj fs)) = Dsj (map (cnf.Neg) fs)
cnf (Neg (Dsj fs)) = Cnj (map (cnf.Neg) fs) 

toSingleCNF :: Form -> Form
toSingleCNF x = cnf (nnf (arrowfree x))

toCNF :: [Form] -> [Form]
toCNF x = map cnf (map nnf (map arrowfree x))

--Assignment 4
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
noNestedAndOr (Cnj [_, (Cnj [_])]) = False
noNestedAndOr (Cnj [(Cnj [_]), _]) = False
noNestedAndOr (Cnj fs) = all noNestedAndOr fs
noNestedAndOr (Dsj [_, (Dsj [_])]) = False
noNestedAndOr (Dsj [(Dsj [_]), _]) = False
noNestedAndOr (Dsj [_, (Cnj [_])]) = False
noNestedAndOr (Dsj [(Cnj [_]), _]) = False
noNestedAndOr (Dsj fs) = all noNestedAndOr fs
noNestedAndOr (Impl f1 f2) = noNestedAndOr f1 && noNestedAndOr f2
noNestedAndOr (Equiv f1 f2) = noNestedAndOr f1 && noNestedAndOr f2

checkIsCNF :: Form -> Bool
checkIsCNF x = containsNoArrows result && noNegativeClauses result &&
        areEquivalent result x && noNestedAndOr result
    where result = toSingleCNF x

-- testnow = verboseCheckWith stdArgs {maxSize = 20} (forAll boundedForm satisfiable)
assignment4 = quickCheckWith stdArgs {maxSize = 20, maxSuccess = 1000} (forAll boundedForm checkIsCNF)