module Cipher where

import Data.Char

-- let2Int transforms an alphabet character into a 2-tuple representation of its
-- self. The first value of the tuple is an index in the range [0, 25] which
-- corresponds to the character's position in the alphabet. The second value is
-- a Bool value representing the character's case. True represents upper case;
-- False represent lower case.
--
-- TODO: The function currently assumes alpha characters as input.
let2Int :: Char -> (Int, Bool)
let2Int c
  | isUpper c = (ord c - ord 'A', True)
  | otherwise = (ord c - ord 'a', False)

-- int2Let transforms a 2-tuple representation of an alphabet character into its
-- Char representaton. It simply reverses the effects of let2int.
--
-- TODO: The function currently assumes alpha characters as input.
int2Let :: (Int, Bool) -> Char
int2Let (n, b)
  | b         = chr $ n + ord 'A'
  | otherwise = chr $ n + ord 'a'

-- shift rotates an alphabet character n positions to the right, accounting for
-- overflow. A negative n shifts to the left.
shift :: Int -> Char -> Char
shift n c = int2Let (mod (fst tup + n) 26, snd tup)
  where tup = let2Int c

-- ceasar encrypts a message using a right-shifted caesar cipher
-- @param  - Int    - the amount to shift each character of the preimage
-- @param  - String - the preimage
-- @return - String - the ciphertext
caesar :: Int -> String -> String
caesar n s = map f s
  where f c
          | isAlpha c = shift n c
          | otherwise = c

-- unceasar decripts a caesar cipher encrypted message.
-- @param  - Int    - the amount each character was shifted
-- @param  - String - the ciphertext
-- @return - String - the preimage
uncaesar :: Int -> String -> String
uncaesar n s = caesar (-n) s
