module Assignment5 where
import Lecture6
import Assignment4

{------------------------------------------------------------------------------

  Assignment 5

  Hours spent: 0.5
------------------------------------------------------------------------------}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

assignment5 = do
    recurseAssignment4 carmichael 1
    recurseAssignment4 carmichael 2
    recurseAssignment4 carmichael 3
    recurseAssignment4 carmichael 4
    recurseAssignment4 carmichael 6
    recurseAssignment4 carmichael 8
    recurseAssignment4 carmichael 10
    recurseAssignment4 carmichael 15
    recurseAssignment4 carmichael 20
{--
"the prime check was fooled by: 294409"
"the prime check was fooled by: 294409"
"the prime check was fooled by: 294409"
"the prime check was fooled by: 294409"
"the prime check was fooled by: 294409"
"the prime check was fooled by: 56052361"
"the prime check was fooled by: 56052361"
"the prime check was fooled by: 294409"
"the prime check was fooled by: 56052361"
--}