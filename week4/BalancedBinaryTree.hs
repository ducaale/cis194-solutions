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

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h l v r)
  | height l < height r = let newL = (insert x l) in Node (succ . height $ newL) newL v r
  | otherwise           = let newR = (insert x r) in Node (succ . height $ newR) l v newR

