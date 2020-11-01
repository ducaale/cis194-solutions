toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev num
  | num < 1   = []
  | num < 10  = [num]
  | otherwise = (num `rem` 10) : toDigitsRev (num `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse
  where
    doubleEveryOther' []       = []
    doubleEveryOther' [x]      = [x]
    doubleEveryOther' (x:y:xs) = x : (y * 2) : (doubleEveryOther' xs)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate = isDivisibleByTen . sumDigits . doubleEveryOther . toDigits
  where
    isDivisibleByTen num = (num `rem` 10) == 0
