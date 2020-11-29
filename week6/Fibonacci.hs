fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = iterate2 (+) 0 1
  where
    iterate2 fn x y = x : iterate2 fn y (fn x y)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = streamMap temp (streamFromSeed succ 1)
  where
    -- TODO: figure out a name shorter than `largest power of 2 which evenly divides n`
    temp n
      | odd n     = 0
      | otherwise = 1 + temp (n `div` 2)

-- TODO : solve remaining optional parts

