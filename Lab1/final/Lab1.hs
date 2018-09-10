{-------------------------------------------------------
    Assignment 1 part 1
--------------------------------------------------------}

-- 1^2 + 2^2 + .. + n^2
sumSquares :: Int -> Int
sumSquares n = sum [x ^ 2 | x <- [1..n]]

-- n(n + 1)(2n +1) / 6
fastSumSquares :: Int -> Int
fastSumSquares n = div (n * (n + 1) * (2 * n + 1)) 6

-- quickcheck
checkWorkshop2 = quickCheck (\ (Positive x) -> sumSquares x == fastSumSquares x)

{-------------------------------------------------------
    Assignment 1 part 2
--------------------------------------------------------}

-- 1^3 + 2^3 + .. + n^3
lsideb :: Int -> Int
lsideb n = sum [x ^ 3 | x <- [1..n]]

-- (n(n+1) / 2)^2
rsideb :: Int -> Int
rsideb n = div  (n * (n + 1)) 2 ^ 2

checkWorkshop3 = quickCheck (\ (Positive x) -> lsideb x == rsideb x)
{-------------------------------------------------------
    Assignment 2

    Answers:
    It is hard to test, since there is a very big increase in things to compute and thus the test runs
    quite slow.

    Checking if 2 to the power of x is the same as the cardinal of the power set of A is being tested.

    Of course it is needed that the subsequences functions works as intended,
    but that's is out of scope for this particular test and should be tested in a separate test
--------------------------------------------------------}

-- |A| = n
setA :: Int -> Int
setA i = length (subsequences [1..i])

-- |>A| = 2^n
subSets :: Int -> Int
subSets i = 2 ^ i

checkWorkshop4 = quickCheck (\ (Positive x) -> setA x == subSets x)

{-------------------------------------------------------
    Assignment 3

    Answers:
    It is hard to test, since there is a very big increase in things to compute and thus the test runs
    quite slow.

    Checking if the factorial of x is the same as the formula in the assignment is being tested

    Of course it is needed that the perms functions works as intended,
    but that's is out of scope for this particular test and should be tested in a separate test
--------------------------------------------------------}

numberPerm, numberObjects :: Int -> Int
numberPerm n = length (permutations [1..n])
numberObjects n = product [1..n]

checkWorkshop5 = quickCheckResult (\ (Positive x) -> (x < 10) --> numberPerm x == numberObjects x)

{-------------------------------------------------------
    Assignment 4

    Answers: By creating a list of all the reversable primes and checking if the tests passes for these
    and the tests do not pass for other numbers.
--------------------------------------------------------}

reversablePrime :: Integer -> [Integer]
reversablePrime x = [x | prime (reversal x)]

filterReversablePrimes :: [Integer] -> [Integer] -> [Integer]
filterReversablePrimes (x:xs) res = if x < 10000 
                                    then filterReversablePrimes xs (res ++ (reversablePrime x))
                                    else res

generateReversablePrimes :: [Integer]
generateReversablePrimes = filterReversablePrimes primes []

{-------------------------------------------------------
    Assignment 5

    Answer: The total is 37447 and covers the primes range of (83 - 677)
    To verify it might be possible to test the prime generator by checking if every number is a prime.
--------------------------------------------------------}

listOfhundredOnes :: [[Int]]
listOfhundredOnes = [takePrimesFrom 101 x | x <- [1..]]
assignment5 = sum (filter (\xs -> prime(sum xs)) listOfhundredOnes !! 1)

{-------------------------------------------------------
    Assignment 6

    Answers: The smalles counter example is 30031
--------------------------------------------------------}

takePrimes :: Int -> [Int]
takePrimes x = take x primes
assignment6 = (filter (\x -> not (prime(x))) [(product (takePrimes x) + 1) | x <- [1..]]) !! 0

{-------------------------------------------------------
    Assignment 7
--------------------------------------------------------}

doDouble :: Char -> Char
doDouble num = if x >= 10 
               then intToDigit (x-9) 
               else intToDigit x where x = digitToInt num * 2

decideDouble :: Int -> Char -> Char
decideDouble n num = if mod n 2 == 0 
                     then doDouble num 
                     else num

calculateSumDigits :: Int -> [Char] -> [Char] -> [Char]
calculateSumDigits n [] res = res
calculateSumDigits n (x:xs) res = calculateSumDigits (n+1) xs (res ++ [decideDouble n x])

luhnMiddle :: [Char] -> [Int]
luhnMiddle number = map digitToInt x where x = calculateSumDigits 1 (reverse number) []

luhnAlgorithm :: [Char] -> Bool
luhnAlgorithm number = mod (sum (luhnMiddle number) * 9) 10 == 0

luhn :: Integer -> Bool
luhn number = luhnAlgorithm (show number)

masterCheckOne :: Integer -> Bool
masterCheckOne number = x >= 2221 && x <= 2720 where x = div number 1000000000000

masterCheckTwo :: Integer -> Bool
masterCheckTwo number = x >= 51 && x <= 55 where x = div number 100000000000000
                    
isMaster :: Integer -> Bool
isMaster number = luhn number && (masterCheckOne number || masterCheckTwo number)

visaCheck :: Integer -> Bool
visaCheck number = x == "4" where x = take 1 (show number)

isVisa :: Integer -> Bool
isVisa number = luhn number && visaCheck number

aeCheck :: Integer -> Bool
aeCheck number = x == "34" || x == "37" where x = take 2 (show number)

isAmericanExpress :: Integer -> Bool
isAmericanExpress number = luhn number && aeCheck number

{-------------------------------------------------------
    Assignment 8
--------------------------------------------------------}

accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x /= Carl) && (x /= Matthew)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not(accuses Matthew x) && not(accuses Peter x)
accuses Arnold x = accuses Matthew x /= accuses Peter x
accuses Carl x = not (accuses Arnold x)

accusers :: Boy -> [Boy]
accusers x = [n | n <- boys, accuses n x]

-- 3 boys are telling the truth, they are accusing the guilty boy.
guilty, honest :: [Boy]
guilty = [n | n <- boys, length (accusers n) == 3]
honest = accusers (head guilty)
