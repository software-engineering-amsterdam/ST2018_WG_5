module Ex2 where

import Data.List
import System.Random
--import Lecture5
import Lecture5Nrc
import Lecture5Refactor

{------------------------------------------------------------------------------

    Assignment 2

    Hours spent: xh

    State: Works

    Todo:
    - Check for further improvements (???)

------------------------------------------------------------------------------}

benchOrig :: Integer -> IO ()
benchOrig x
    | x == 0 = print "done with orig solution"
    | otherwise = do
        r <- Lecture5Nrc.genRandomSudoku
        s <- Lecture5Nrc.genProblem r
        benchOrig (x - 1)

benchRef :: Integer -> IO ()
benchRef x
    | x == 0 = print "done with refactor solution"
    | otherwise = do
        r <- Lecture5Refactor.genRandomSudoku
        s <- Lecture5Refactor.genProblem r
        benchRef (x - 1)

-- Not sure iff asked for but this is how you do it with the refactored version.
benchRefNrc :: Integer -> IO ()
benchRefNrc x
    | x == 0 = print "done with NRC refactor solution"
    | otherwise = do
        r <- Lecture5Refactor.genRandomNrcSudoku
        s <- Lecture5Refactor.genProblemNrc r
        benchRefNrc (x - 1)        

-- RUN WITH
-- :set +s
origtest = benchOrig 10 -- (1.66 secs, 878,783,760 bytes)
-- SLOW FIXME? (replace all those crappy list join/nubs?)
reftest = benchRef 10 -- (44.48 secs, 11,455,142,792 bytes)
--
--nrcreftest = benchRefNrc 10

