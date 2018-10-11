module Lab6 where

import Data.Bits
import Test.QuickCheck

{------------------------------------------------------------------------------

  Assignment 1

  Hours spent: 1
  Answer:
------------------------------------------------------------------------------}

exM :: Integer -> Integer -> Integer -> Integer
exM 0 0 _ = 0
exM _ 0 _ = 1
exM b e m = t * exM ((b * b) `mod` m) (shiftR e 1) m `mod` m where t = if testBit e 0 then b `mod` m else 1

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 0.5
  Answer:
------------------------------------------------------------------------------}

-- time ./testExM
--real    0m0,008s
--user    0m0,004s
--sys     0m0,004s

-- time ./testNormal
-- doesn't finish within a minute

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent:
  Answer:
------------------------------------------------------------------------------}


