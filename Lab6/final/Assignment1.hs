module Assignment1 where
import Lecture6

{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: 1.5
  Answer:
    Inspiration from:
    https://www.geeksforgeeks.org/modular-exponentiation-power-in-modular-arithmetic/

    (copy pasted it as well to the Lecture file)
------------------------------------------------------------------------------}
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' x y p = if even y then current else (x * current) `mod` p
                where
                    current = (remaining * remaining) `mod` p
                    remaining = exM' x (y `div` 2) p
{- Output:
    *Assignment1> exM' 2 3 5
    3

    *Assignment1> exM' 2 5 13
    6
-}

