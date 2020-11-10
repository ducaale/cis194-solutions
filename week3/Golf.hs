module Golf where

import Data.List  (group, sort)
import Data.Maybe (fromMaybe)

skips :: [a] -> [[a]]
skips string = map (\n -> skip n string) [1 .. length string]
  where
    -- takses a word and filters chars whose indices is divisable by n
    skip n = map snd . filter ((\x -> x `mod` n == 0) . fst) . zip [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs) =
  if x1 < x2 && x2 > x3 then x2 : remaining else remaining
  where
    remaining = localMaxima (x2:x3:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram data' = histogram' (freqFromZeroToNine data') ++ "\n" ++ footer
  where
    freq = map (\x -> (head x, length x)) . group . sort
    freqFromZeroToNine nums = map (\x -> fromMaybe 0 . lookup x . freq $ nums) [0..9]
    footer = (replicate 10 '=') ++ "\n" ++ ['0'..'9']
    histogram' nums
      | all (< 1) nums = []
      | otherwise =
         (histogram' $ map pred nums) ++ "\n" ++ (map (\n -> if n > 0 then '*' else ' ') nums)

