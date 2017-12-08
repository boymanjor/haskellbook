module BTree where

data BTree a = Lf | Nd (BTree a) a (BTree a) deriving (Eq, Ord, Show)

insert :: Ord a
       => a
       -> BTree a
       -> BTree a
insert x Lf = Nd Lf x Lf
insert x (Nd l v r)
  | x == v = Nd l v r
  | x < v  = Nd (insert x l) v r
  | otherwise = Nd l v (insert x r)

mapt :: (a -> b) -> (BTree a) -> (BTree b)
mapt _ Lf = Lf
mapt f (Nd l v r) =
  (Nd (mapt f l) (f v) (mapt f r))

inorder :: BTree a -> [a]
inorder Lf         = []
inorder (Nd l v r) = concat [inorder l, [v], inorder r]

preorder :: BTree a -> [a]
preorder Lf         = []
preorder (Nd l v r) = concat [[v], inorder l, inorder r]

postorder :: BTree a -> [a]
postorder Lf         = []
postorder (Nd l v r) = concat [inorder l, inorder r, [v]]

foldt :: (a -> b -> b)
         -> b
         -> BTree a
         -> b
foldt _ z Lf           = z
foldt f z (Nd Lf v Lf) = f v z
foldt f z (Nd l v r)   = foldt f (f v (foldt f z r)) l
