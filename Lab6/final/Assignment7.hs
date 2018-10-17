module Assignment7 where

import Data.List
import System.Random
import Lecture6
import Assignment5
import Assignment4

{------------------------------------------------------------------------------

Assignment 7

Hours spent: 1h

Output:
"Original message: 123456789"
"Encoded message: 28679718602997181072337614380936720482949"
"Decoded message: 123456789"
------------------------------------------------------------------------------}

-- Returns a prime value in the range
-- between 2^(n-1) and 2^(n)
genNBitPrime :: Int -> IO (Integer)
genNBitPrime n = do
    -- stay in range
    p <- randomRIO((2 ^ (n - 1)) + 1, (2 ^ n) - 1)
    isPrime <- primeMR 10 p
    if isPrime
        then return p
        else genNBitPrime n

-- Returns a non identical pair of prime values 
-- between 2^(n-1) and 2^(n)
rsaPrimePair :: Int -> IO (Integer, Integer)
rsaPrimePair n = do
    p <- genNBitPrime n
    q <- genNBitPrime n
    -- check iff unique
    if (p /= q)
        then return (p, q)
        else rsaPrimePair n

ass7 = do
    let nbits = 128
    -- Find 2 random and distinct prime numbers p and q.
    pq <- rsaPrimePair nbits
    let publicKey = rsaPublic (fst pq) (snd pq)
    let privateKey = rsaPrivate (fst pq) (snd pq)
    -- message
    let msg = 123456789
    print ("Original message: " ++ show msg)
    let msgEncode = rsaEncode publicKey msg
    print ("Encoded message: " ++ show msgEncode)
    let msgDecode = rsaDecode privateKey msgEncode
    print ("Decoded message: " ++ show msgDecode)