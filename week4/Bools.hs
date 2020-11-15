xor :: [Bool] -> Bool
xor = foldr (\x accum -> if x then not accum else accum) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x accum -> f x : accum) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (\x accum -> f accum x) base

