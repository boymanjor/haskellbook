module TopOrLocal where

-- In this example, 'topLevelValue' has access to the entire module.
-- 'woot', otoh, is only defined in the scope of 'topLevelFunction'.
topLevelFunction :: Integer -> Integer
topLevelFunction x =
  x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5
