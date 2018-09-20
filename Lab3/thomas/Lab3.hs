module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
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
-- TODO:

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

toCNF :: [Form] -> [Form]
toCNF x = map cnf (map nnf (map arrowfree x))

-- Assignment 4
