import Data.List

uniqueCartProd :: Ord a => [a] -> [a] -> [(a, a)]
uniqueCartProd xs ys = [(x,y) | x <- xs, y <- ys, x <= y]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> (x * 2) + 1) ([1..n] \\ sieve)
  where
    sieve =
      filter (<= n)
      . map (\(i, j) -> i + j + (2 * i * j))
      . uniqueCartProd [1..n] $ [1..n]

