module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{------------------------------------------------------------------------------

  Assignment 1

  Answer:
    - The Russel Paradox is not entirely clear to me.
    - The halts function is not very clear to me.
    - Page 136
    - Notation in example 4.6 doesn’t really make the point clear for me
    - Exercise 4.26 and 4.27. Coming up with proof (proof in general)
    - What is a real world use case of a complement?
    - Are there other ways to represent sets in Haskell which do not use lists?

    -- Remark. The question whether the equation a = { a } has solutions (or, more
    -- generally,whether sets a exist such that a ∈ a) is answered differentlyby different
    -- axiomatizations of set theory.
    --
    -- What does this mean?

    -- Page 137
    -- Exercise 4.11 Explain that ∅ != {∅}
    -- Is that because {∅} is not empty because it has an empty element?

    -- Page 149
    -- Exercise 4.40 1. Assume that A and B are non-empty and that A × B = B × A. Show that A = B.
    -- 2. Show by means of an example that the condition of non-emptiness in 1 is necessary. (Did you use this in your proof of 1?)
    --
    -- I think i can solve this problem using a very simple case where A = B = {1,2}
    -- But how can I verify that my simple example is enough to proof that non-emptiness in 1 is necessary

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
    - Why is R = {(1,4),(1,5),(2,5)} is a relation from {1,2,3} to {4,5,6}
      and not a relation from {1,2} to {4,5}?
    - What is the function of the identity relation?
    - Why are the functions curry and uncurry fundamental for functional
      programming?
    - Again coming up with proof in general
    - 5.63,5.64,6.65 is not clear for me
    - Partitions. And especially such notation used at 5.83

    -- Page 200
    -- Example 5.63 The relation ∼ between vectors in 3-dimensional space R 3 that is
    -- defined by~ a ∼ ~ b ≡ ∃r ∈ R + (~ a = r ~ b) is an equivalence.
    --
    -- What does the ∼ symbol mean between the two vectors? Never seen it before in this book

    -- Page 213
    -- Integer Partitions
    -- I cannot figure out for what kind of problems integer partitions can be usefull (ignoring the cash change examples)

    -- Page 204
    -- Equivalence classes
    -- I dont see why this needs to be a new class.

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

