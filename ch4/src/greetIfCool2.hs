module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyyyyy. What's shakin'?"
  else
    putStrLn "psshhh"
  where cool x =
          x == "downright frosty yo"
