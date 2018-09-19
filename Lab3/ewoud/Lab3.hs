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

    STATE: 

------------------------------------------------------------------------------}