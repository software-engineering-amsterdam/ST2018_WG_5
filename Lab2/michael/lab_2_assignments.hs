module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


{------------------------------------------------------------------------------

  Assignment 1
  Your programmer Red Curry...

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1)
            return (p:ps)

-- TODO

{------------------------------------------------------------------------------

  Assignment 2
  Recognizing triangles

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)
-- TODO

{------------------------------------------------------------------------------

  Assignment 3
  Testing properties strength

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p
-- TODO

{------------------------------------------------------------------------------

  Assignment 4
  Recognizing Permutations

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO

{------------------------------------------------------------------------------

  Assignment 5
  Recognizing and generating derangements

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO

{------------------------------------------------------------------------------

  Assignment 6
  Implementing and testing ROT13 encoding

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO

{------------------------------------------------------------------------------

  Assignment 7
  Implementing and testing IBAN validation

  Hours spent: TODO
  Answers:
  - TODO

------------------------------------------------------------------------------}
-- TODO







--
