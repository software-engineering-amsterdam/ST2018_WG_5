import Criterion.Main
import Lecture6
import Lab6

-- ewouddd ik snap het niet
-- Lees deze 2 links voor kleine uitleg criterion benchmark + installatie:
-- http://www.serpentine.com/criterion/tutorial.html
-- https://ocharles.org.uk/posts/2012-12-18-24-days-of-hackage-criterion.html
--
-- En deze link als je arguments niet snapt:
-- https://stackoverflow.com/questions/31678184/haskell-criterion-nf-is-applied-to-too-few-arguments

-- Compile: ghc -O --make bench2
-- Run: ./bench2 --output bench2.html
-- or ./bench2


bench_Expm :: (Integer, Integer, Integer) -> Integer
bench_Expm (x, y, z) = Lecture6.expM x y z

bench_Exm :: (Integer, Integer, Integer) -> Integer
bench_Exm (x, y, z) = Lab6.exM x y z

-- Our benchmark harness.
main = defaultMain [
    -- Case: Increase exponent
    bgroup "orig_expIncr" [ bench "10^10 mod 123" $ nf bench_Expm (10, 10, 123)
                , bench "10^100 mod 123" $ nf bench_Expm (10, 100, 123)
                , bench "10^1000 mod 123" $ nf bench_Expm (10, 1000, 123)
                , bench "10^10000 mod 123" $ nf bench_Expm (10, 10000, 123)
    ],
    bgroup "opti_expIncr" [ bench "10^10 mod 123" $ nf bench_Exm (10, 10, 123)
                , bench "10^100 mod 123" $ nf bench_Exm (10, 100, 123)
                , bench "10^1000 mod 123" $ nf bench_Exm (10, 1000, 123)
                , bench "10^10000 mod 123" $ nf bench_Exm (10, 10000, 123)
    ],
    -- Case: Increase base
    bgroup "orig_baseIncr" [bench "10^10 mod 123" $ nf bench_Expm (10, 10, 123)
                , bench "100^10 mod 123" $ nf bench_Expm (100, 10, 123)
                , bench "1000^10 mod 123" $ nf bench_Expm (1000, 10, 123)
                , bench "10000^10 mod 123" $ nf bench_Expm (10000, 10, 123)                 
    ],
    bgroup "opti_baseIncr" [ bench "10^10 mod 123" $ nf bench_Exm (10, 10, 123)
                , bench "100^10 mod 123" $ nf bench_Exm (100, 10, 123)
                , bench "1000^10 mod 123" $ nf bench_Exm (1000, 10, 123)
                , bench "10000^10 mod 123" $ nf bench_Exm (10000, 10, 123)                 
    ]
  ]