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
-- NOTE: RESERACH: Not all logical consequences are tautological consequences (what does this mean here???, source wikipedia)
entails :: Form -> Form -> Bool
-- q is a tautological entailment of p if for all outcomes for the two statemens there is no
-- outcome where p is true and q is false. 
entails p q = tautology (Impl p q)

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
    Time:  min

    STATE: Test if logic is correct

------------------------------------------------------------------------------}

-- info used:
-- https://ocw.mit.edu/courses/health-sciences-and-technology/hst-947-medical-artificial-intelligence-spring-2005/lecture-notes/ch10_logic2a.pdf
-- https://math.stackexchange.com/questions/214338/how-to-convert-to-conjunctive-normal-form

-- Applies the 2 distributive laws when applicable
-- (P⋁(Q⋀R)) ↔ (P⋁Q)⋀(P⋁R) 
-- and
-- (P⋀(Q⋁R)) ↔ (P⋀Q)⋁(P⋀R)
distributiveLaws :: Form -> Form 
-- Same as nnf
distributiveLaws (Prop x) = Prop x
distributiveLaws (Neg (Prop x)) = Neg (Prop x)
distributiveLaws (Neg (Neg f)) = nnf f
-- (P⋁(Q⋀R)) ↔ (P⋁Q)⋀(P⋁R)
distributiveLaws (Dsj [p, (Cnj [q, r])]) = Cnj [(Dsj [p, q]), (Dsj [p, r])]
-- (P⋀(Q⋁R)) ↔ (P⋀Q)⋁(P⋀R)
distributiveLaws (Cnj [p, (Dsj [q, r])]) = Dsj [(Cnj [p, q]), (Cnj [p, r])]

-- Functions as:
-- 1) Eliminate arrows using definitions : arrowfree
-- 2) Drive in negation using de morgans law : nnf
-- 3) Distributive Laws to distribute or over and : distributiveLaws
convertToCnf :: Form -> Form
convertToCnf x = distributiveLaws $ nnf $ arrowfree x

--assignment3 = do

{------------------------------------------------------------------------------
    Assignment 4
    Time:  min

    STATE: ik ga dit in quickcheck doen, weet nog neit hoe

------------------------------------------------------------------------------}