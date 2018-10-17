module Lab6 where

import Data.Bits
import Test.QuickCheck
import Lecture6

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
-- Doesn't finish within a minute

{------------------------------------------------------------------------------

  Assignment 3

  Hours spent: 0.25
  Answer:
------------------------------------------------------------------------------}

composites' :: [Integer]
composites' = [x | x <- [2..], not $ prime x]

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 1
  Answer:
  As k increases the found composites are larger.
------------------------------------------------------------------------------}

testFermat k (x:xs) = do
   failureFound <- primeTestsF k x
   if failureFound then print $ x else testFermat k xs

ass4 = do
      testFermat 1 composites'
      testFermat 2 composites'
      testFermat 3 composites'
      testFermat 4 composites'
      testFermat 5 composites'

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: 0.5
  Answer:
------------------------------------------------------------------------------}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

ass5 = do
      testFermat 1 carmichael
      testFermat 2 carmichael
      testFermat 3 carmichael
      testFermat 4 carmichael
      testFermat 5 carmichael

{------------------------------------------------------------------------------

  Assignment 6 1

  Hours spent: 0.5
  Answer:
  These tests do not finish within 5 minutes.
------------------------------------------------------------------------------}

testMillerRubin k (x:xs) = do
   failureFound <- primeMR k x
   if failureFound then print $ x else testMillerRubin k xs

ass61 = do
      testMillerRubin 1 carmichael
      testMillerRubin 2 carmichael
      testMillerRubin 3 carmichael
      testMillerRubin 4 carmichael
      testMillerRubin 5 carmichael

{------------------------------------------------------------------------------

  Assignment 6 2

  Hours spent:
  Answer:

------------------------------------------------------------------------------}


