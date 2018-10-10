module Lab6 where

import Data.List
import System.Random
import Numeric
import Data.Bits
-- not sure about this import atm
import Lecture6 (prime)


{------------------------------------------------------------------------------

Assignment 1

Hours spent: x

Idea: Right-to-left binary method aka modular exponentiation with repeated squaring
(https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method)
(https://nptel.ac.in/courses/106103015/10)
------------------------------------------------------------------------------}

-- Exm:: base(b) -> exponent(e) -> modulus(m) -> result
exM :: Integer -> Integer -> Integer -> Integer
exM b 0 m = 1
exM b e m = mult * exM ((b * b) `mod` m) (shiftR e 1) m `mod` m where
    -- if bit value [,,x,,] is 0, do not modify result
    -- otherwise set result
    mult    | testBit e 0 = b `mod` m
            | otherwise = 1


{------------------------------------------------------------------------------

Assignment 2

Hours spent: x

------------------------------------------------------------------------------}

-- See: bench2.hs + bench2.html

{------------------------------------------------------------------------------

Assignment 3

Hours spent: x

------------------------------------------------------------------------------}

-- A composite number n is a positive integer n>1 which is not prime (i.e., which has factors other than 1 and itself). 
-- The first few composite numbers (sometimes called "composites" for short) are 4, 6, 8, 9, 10, 12, 14, 15, 16, ... 
-- source: http://mathworld.wolfram.com/CompositeNumber.html

composites :: [Integer]
composites = filter (\x -> not (prime x)) [2..]--filter (not . prime) [2..]

{------------------------------------------------------------------------------

Assignment 4

Hours spent: x

------------------------------------------------------------------------------}