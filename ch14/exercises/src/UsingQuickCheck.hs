module UsingQuickCheck where

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered = snd . foldr fold (Nothing, True)
  where
    fold _ status@(_, False) = status
    fold y (Nothing, b) = (Just y, b)
    fold y (Just x, _) = (Just y, x >= y)

reverseIdentity :: [a] -> [a]
reverseIdentity = reverse . reverse
