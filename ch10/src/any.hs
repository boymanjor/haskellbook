any' :: (a -> Bool) -> [a] -> Bool
any' f xs =
  foldr (\x b -> f x || b) False xs

-- example execution on infinite list
--
-- any' even [1..]
--   = foldr (\x b -> f x || b) False [1..]
--   = foldr f' False [1..]
--
-- foldr f' False [1..]
--   = f' 1 (foldr f' False [2..])
--   = even 1 || (foldr f' False [2..])
--   = foldr f' False [2..]
--
-- foldr f' False [2..]
--   = f' 2 (foldr f' False [3..])
--   = even 2 || (foldr f' False [3..])
--   = True


