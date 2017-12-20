module CipherTest where

import Cipher

prop_caesar

main :: IO ()
main = do
  quickCheck
  quickCheck
