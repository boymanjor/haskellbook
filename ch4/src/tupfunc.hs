module TupFunc where

topFunc :: (Int, [a])
        -> (Int, [a])
        -> (Int, [a])
topFunc (a, b) (c, d) =
  (a + c, b ++ d)
