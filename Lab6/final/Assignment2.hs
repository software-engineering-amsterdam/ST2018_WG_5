module Assignment2 where

import Data.List
import System.Random
import Lecture6
import Assignment1

{------------------------------------------------------------------------------

    Assignment 2

    Hours spent: 1h

    See bench2.html for more results.

    Increasing the exponent
    original expm:
    10^10 mod 123 : mean 245.8 ns   (243.7 ns .. 250.3 ns)
    10^100 mod 123 : mean 522.4 ns   (521.3 ns .. 523.3 ns)
    10^1000 mod 123 : mean 1.608 μs   (1.599 μs .. 1.630 μs)
    10^10000 mod 123 : mean 29.79 μs   (29.73 μs .. 29.96 μs)

    custom exM
    10^10 mod 123 : mean 382.4 ns   (382.0 ns .. 383.2 ns)
    10^100 mod 123 : mean 639.6 ns   (631.9 ns .. 668.2 ns
    10^1000 mod 123 : mean 1.074 μs   (1.048 μs .. 1.114 μs)
    10^10000 mod 123 : mean 1.253 μs   (1.242 μs .. 1.275 μs)

    The exM implementation scales much better with an increasing exponent compared
    to the original implementation.

    Increasing the base
    original expm
    10^10 mod 123 : mean 246.7 ns   (245.8 ns .. 248.7 ns)
    100^10 mod 123 : mean 255.3 ns   (251.3 ns .. 265.2 ns)
    1000^10 mod 123 : mean 271.6 ns   (270.2 ns .. 274.8 ns)
    10000^10 mod 123 : mean 296.8 ns   (295.3 ns .. 300.3 ns)

    custom expm
    10^10 mod 123 : mean 404.5 ns   (392.7 ns .. 423.5 ns)
    100^10 mod 123 : mean 378.3 ns   (377.6 ns .. 379.0 ns)
    1000^10 mod 123 : mean 383.1 ns   (380.9 ns .. 387.9 ns)
    10000^10 mod 123 : mean 392.6 ns   (386.3 ns .. 405.7 ns)

    Both implementations show similar performance with an increased base
------------------------------------------------------------------------------}

-- See: bench2.hs + bench2.html