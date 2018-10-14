module Assignment3 where

import Lecture6

{------------------------------------------------------------------------------
  Assignment 3
  Took: 0.5 hours
  A number which is bigger than one and has is not a prime is composite number.
------------------------------------------------------------------------------}

composites' :: [Integer]
composites' = filter (not . prime) [2..]