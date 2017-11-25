module Head where

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:[]) = Just x
safeHead (x:_)  = Just x
