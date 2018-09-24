module Lab3 where

import Data.List
import Test.QuickCheck
import Control.Monad
import Lecture3

{------------------------------------------------------------------------------
    Assignment X
    Time: X min

    STATE: X
------------------------------------------------------------------------------}

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
    Time:  min

    STATE: wtf willen ze gewoon iets heel simpels zoals dit?

------------------------------------------------------------------------------}

-- idee?
assignment2 = do
    print ((parse "+(1 2)") == ([Dsj [Prop 1, Prop 2]]))


{------------------------------------------------------------------------------
    Assignment 3
    Time:  90 min

    STATE: WORKS? If i use michaels quickcheckWith idea it is successfull.

------------------------------------------------------------------------------}

-- info used:
-- https://ocw.mit.edu/courses/health-sciences-and-technology/hst-947-medical-artificial-intelligence-spring-2005/lecture-notes/ch10_logic2a.pdf
-- https://math.stackexchange.com/questions/214338/how-to-convert-to-conjunctive-normal-form
--
-- reminder:
-- "∨" is disjunction / OR
-- "∧" is conjunction / AND

-- Applies the 2 distributive laws when applicable
-- (P⋁(Q⋀R)) ↔ (P⋁Q)⋀(P⋁R) 
-- and
-- (P⋀(Q⋁R)) ↔ (P⋀Q)⋁(P⋀R)
distLaws :: Form -> Form -> Form 
distLaws (Cnj []) f2 = Cnj []
distLaws f1 (Cnj []) = Cnj []
distLaws (Cnj (f1:[])) f2 = distLaws f1 f2
distLaws f1 (Cnj (f2:[])) = distLaws f1 f2
distLaws (Cnj (f1:fs)) f2 = Cnj [distLaws f1 f2, distLaws (Cnj fs) f2]
distLaws f1 (Cnj (f2:fs)) = Cnj [distLaws f1 f2, distLaws f1 (Cnj fs)]
distLaws f1 f2 = Dsj [f1,f2]

cnf :: Form -> Form
cnf (Dsj []) = Dsj []
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Dsj (fs:[])) = cnf fs
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj (f1:f2:fs)) = distLaws (cnf f1) (cnf (Dsj(f2:fs)))

-- Functions as:
-- 1) Eliminate arrows using definitions : arrowfree
-- 2) Drive in negation using de morgans law : nnf
-- 3) Distributive Laws to distribute or over and : distributiveLaws
convertToCnf :: Form -> Form
convertToCnf x = cnf $ nnf $ arrowfree x

testform = (Equiv (Prop 3)(Dsj [Prop 4, Prop 5]))

-- quickcheck?
prop_cnf :: Form -> Bool
prop_cnf p  =  equiv p (convertToCnf p)

--
--assignment3 = quickCheckResult prop_cnf
-- dit heb ik van michael gestolen, hij passed
assignment3 = quickCheckWith stdArgs {maxSize = 3, maxSuccess = 50} prop_cnf

{------------------------------------------------------------------------------
    Assignment 4
    Time: 120 min

    STATE: Generator werkt?

------------------------------------------------------------------------------}

--  sample $ (arbitrary :: Gen Form)
-- https://hackage.haskell.org/package/QuickCheck-2.10.1/docs/Test-QuickCheck-Gen.html
instance Arbitrary Form where
    arbitrary  =  sized form
        where 
            form n 
                | n <= 0 = do
                    t  <- arbitrary
                    return (Prop t)
                | otherwise = oneof [ liftM Neg formA
                                    , liftM Dsj formClosed
                                    , liftM Cnj formClosed
                                    , liftM2 Impl formA formB
                                    , liftM2 Equiv formA formB
                                    ]
                    where
                        -- howto deal with longgggg stuff?
                        formA = form (n `div` 5)
                        formB = form (n `div` 6)
                        formClosed = listOf1 (form (n `div` 10))
