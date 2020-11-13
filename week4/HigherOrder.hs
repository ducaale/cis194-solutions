import Data.List (foldl')

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . collatzSeries
  where
    collatzSeries =
      takeWhile (> 1) . iterate (\n -> if even n then (n `div` 2) else (3 * n + 1))

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node depth leftTree value rightTree) =
      if x < value
      then rebalance $ Node (depth + 1) (insert x leftTree) value rightTree
      else rebalance $ Node (depth + 1) leftTree value (insert x rightTree)

    rebalance Leaf = Leaf
    rebalance (Node _ rightTree _ leftTree) = undefined

