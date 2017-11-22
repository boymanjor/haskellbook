module RegisteredUser where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

-- pretty print User values
printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser
           (Username name)
           (AccountNumber acctNo)) =
  putStrLn $ concat [name, " ", show acctNo]
