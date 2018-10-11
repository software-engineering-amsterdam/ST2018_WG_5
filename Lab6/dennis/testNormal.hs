module Main where

import Lab6

main = do
  print (mod (10^10) 3)
  print (mod (100^100) 3)
  print (mod (1000^1000) 3)
  print (mod (10000^10000) 3)
  print (mod (100000^100000) 3)
  print (mod (123432534^123432534) 3)
  print (mod (11^47) 3)