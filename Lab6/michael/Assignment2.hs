module Assignment2 where

import Data.List
import System.Random
import Lecture6
import Assignment1

{------------------------------------------------------------------------------

  Assignment 2

  Hours spent: 0.5
  Answer:
    Used the tip found here: https://stackoverflow.com/questions/6766450/haskell-function-execution-time
    for running every try with ":set +s" turned on. This way we can check how long functions take to complete

    Running a large numbers will be the test to undertake for both implementations. See below which
    effects have taken place. You can see that when the number becomes larger the original solution
    really decreases dramatically in speed. While the improved solution performs these numbers without any
    problem really fast.
------------------------------------------------------------------------------}

assignment2Initial1 = expM 2222 60000000 25
assignment2Initial2 = expM 2222 600000000 25
assignment2Improved1 = exM' 2222 60000000 25
assignment2Improved2 = exM' 2222 600000000 25

{- Output: (with :set +s turned on

    *Assignment2> assignment2Initial1
    1
    (7.18 secs, 229,204,880 bytes)

    *Assignment2> assignment2Initial2
    1
    (95.58 secs, 2,479,277,520 bytes)

    *Assignment2> assignment2Improved1
    1
    (0.00 secs, 92,008 bytes)

    *Assignment2> assignment2Improved2
    1
    (0.00 secs, 93,168 bytes)

-}