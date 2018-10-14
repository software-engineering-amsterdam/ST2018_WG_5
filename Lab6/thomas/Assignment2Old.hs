module Assignment2Old where

import Lecture6

{------------------------------------------------------------------------------
  Assignment 2
  
  Hours spent: 1
  After timing both executables the following results were printed:
  Assignment2Old: TotalSeconds: 4,30509
  Assignment2New: TotalSeconds: 0,27567

  So the new was is indeed faster
------------------------------------------------------------------------------}
testOriginal :: IO ()
testOriginal = do 
                   let a = expM 1234567 8 123
                   let b = expM 8 1234567 123
                   let c = expM 8888 1234567 123
                   let d = expM 1234567 8888 123
                   let e = expM 123432537 12343253 3 
                   let res = (a + b + c + d + e)
                   print (show res)

main = testOriginal