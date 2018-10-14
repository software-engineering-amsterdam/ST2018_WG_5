module Assignment4 where
import Lecture6

{------------------------------------------------------------------------------

  Assignment 4

  Hours spent: 0.5
  Answer:
    
------------------------------------------------------------------------------}

recurseAssignment4 (x:xs) k = do
                                tested <- primeTestsF k x
                                if tested then print ("the prime check was fooled by: " ++ (show x))
                                          else recurseAssignment4 xs k 

assignment4 = do
                recurseAssignment4 composites 1
                recurseAssignment4 composites 2
                recurseAssignment4 composites 3
                recurseAssignment4 composites 4
                recurseAssignment4 composites 6
                recurseAssignment4 composites 8
                recurseAssignment4 composites 10
                recurseAssignment4 composites 15
                recurseAssignment4 composites 20
{--
"the prime check was fooled by: 49"
"the prime check was fooled by: 25"
"the prime check was fooled by: 561"
"the prime check was fooled by: 1729"
"the prime check was fooled by: 1729"
"the prime check was fooled by: 2465"
"the prime check was fooled by: 46657"
"the prime check was fooled by: 252601"
"the prime check was fooled by: 252601"
--}