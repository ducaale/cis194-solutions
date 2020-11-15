import qualified Data.Tree as T

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Eq)

instance Show a => Show (Tree a) where
  show = T.drawTree . toDataTree
    where
      toDataTree Leaf = T.Node "Leaf" []
      toDataTree (Node d l v r) = T.Node (show v ++ "<" ++ show d ++ ">") [toDataTree r, toDataTree l]

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node d leftTree value rightTree)
  | x < value = recomputeDepth $ balance $ Node (d + 1) (insert x leftTree) value rightTree
  | otherwise = recomputeDepth $ balance $ Node (d + 1) leftTree value (insert x rightTree)

balance :: Ord a => Tree a -> Tree a
balance Leaf = Leaf
balance tree@(Node _ leftTree _ rightTree)
  | (depth leftTree) - (depth rightTree) > 1 = rotateRight tree
  | (depth rightTree) - (depth leftTree) > 1 = rotateLeft tree
  | otherwise = tree

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft (Node xd xl xv xr) =
  case xr of
    Leaf -> (Node xd xl xv xr)
    (Node yd yl yv yr) ->
      let x = recomputeDepth (Node 0 xl xv yl) in (Node yd x yv yr)

rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight (Node xd xl xv xr) =
  case xl of
    Leaf -> (Node xd xl xv xr)
    (Node yd yl yv yr) ->
      let x = recomputeDepth (Node 0 yr xv xr) in (Node yd yl yv x)

depth :: Tree a -> Integer
depth Leaf = -1
depth (Node d _ _ _) = d

recomputeDepth :: Tree a -> Tree a
recomputeDepth Leaf = Leaf
recomputeDepth (Node d l v r) = (Node newDepth l v r)
  where
    newDepth = succ $ max (depth l) (depth r)

