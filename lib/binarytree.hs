module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert :: Ord a
       => a
       -> BinaryTree a
       -> BinaryTree a
insert n Leaf = Node Leaf n Leaf
insert n (Node l a r)
  | n == a = Node l a r
  | n < a  = Node (insert n l) a r
  | otherwise = Node l a (insert n r)

mapt :: (a -> b) -> (BinaryTree a) -> (BinaryTree b)
mapt _ Leaf = Leaf
mapt f (Node l a r) =
  (Node (mapt f l) (f a) (mapt f r))

inorder :: BinaryTree a -> [a]
inorder Leaf         = []
inorder (Node l a r) = concat [inorder l, [a], inorder r]

preorder :: BinaryTree a -> [a]
preorder Leaf         = []
preorder (Node l a r) = concat [[a], inorder l, inorder r]

postorder :: BinaryTree a -> [a]
postorder Leaf         = []
postorder (Node l a r) = concat [inorder l, inorder r, [a]]

foldt :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldt _ z Leaf           = z
foldt f z (Node Leaf a Leaf) = f a z
foldt f z (Node l a r)   = foldt f (f a (foldt f z r)) l
