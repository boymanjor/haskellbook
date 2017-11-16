module Reverse where

rvrs :: String -> String
rvrs x =  concat [drop 9 x, take 4 $ drop 5 x, take 5 x]

main :: IO ()
main = print $ rvrs "Curry is awesome"

-- Also works:
-- main = print (rvrs "Curry is awesome")
