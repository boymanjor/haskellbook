module Cipher where

import Data.Char

-- ceasar encrypts a message using a right-shifted caesar cipher
-- @param  - Int    - the amount to shift each character of the preimage
-- @param  - String - the preimage
-- @return - String - the encoded message
caesar :: Int -> String -> String
caesar _ [] = []
caesar x (y:ys)
  | isUpper y = shift 'A' : caesar x ys
  | otherwise = shift 'a' : caesar x ys
  where shift x' = chr ((mod (ord y + x - ord x') 26) + ord x')

-- unceasar decripts a caesar cipher encrypted message.
-- @param  - Int    - the amount each character was shifted
-- @param  - String - the preimage
-- @return - String - the encoded message
uncaesar :: Int -> String -> String
uncaesar _ [] = []
uncaesar x (y:ys)
  | isUpper y = shift 'A' : uncaesar x ys
  | otherwise = shift 'a' : uncaesar x ys
  where shift x' = chr ((mod (ord y - x - ord x') 26) + ord x')
