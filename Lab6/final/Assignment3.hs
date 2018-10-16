module Assignment3 where

import Data.List
import System.Random
import Lecture6

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 0.5
  Answer:
    Using the knowledge that a composite number has 2 conditions:
        - > 1
        - not a prime
    We can create the function that generates composites

    (copy pasted it as well to the Lecture file)
------------------------------------------------------------------------------}
composites' :: [Integer]
composites' = filter (\x -> not (prime x)) [2..]
{- Output:
    *Assignment3> composites
    [4,6,8,9,10,12,14,15,16,18,20,21,22,24,25,26,27,28,30,32,33,34,35,36,38,39,40,42,44,45,46,48,49,50,51,52,54,55,...
-}