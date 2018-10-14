module Assignment1 where
import Lecture6

{------------------------------------------------------------------------------
  Assignment 1
  
  Hours spent: 2
  Answer:
    Based on https://math.stackexchange.com/questions/195634/how-do-you-calculate-the-modulo-of-a-high-raised-number
------------------------------------------------------------------------------}

exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' x y p = if odd y then (x * newbase) `mod` p
                      else newbase
                        where
                            newbase = (base * base) `mod` p
                            base = exM' x (y `div` 2) p

test1 = (expM 439 233 713) == (exM' 439 233 713)
test2 = (expM 7 256 13) == (exM' 7 256 13)