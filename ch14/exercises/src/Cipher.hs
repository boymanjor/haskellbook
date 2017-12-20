module Cipher where

import Control.Monad (forever)
import Data.Char
import System.Exit (exitSuccess)
import System.IO
import Text.Read

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
shift n c
  | isAlpha c  = int2Let (mod (fst tup + n) 26, snd tup)
  | otherwise = c
  where tup = let2Int c

-- ceasar encrypts a message using a right-shifted caesar cipher
caesar :: IO ()
caesar = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "Enter shift amount: "
  mInt <- getLine
  case readMaybe mInt :: Maybe Int of
    Just n -> do
      putStr "Enter preimage: "
      preimage <- getLine
      putStrLn $ map (shift n) preimage
      exitSuccess
    Nothing -> putStrLn "Must enter an integer; please try again."

-- unceasar decripts a caesar cipher encrypted message.
uncaesar :: IO ()
uncaesar = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "Enter shift amount: "
  mInt <- getLine
  case readMaybe mInt :: Maybe Int of
    Just n -> do
      putStr "Enter ciphertext: "
      preimage <- getLine
      putStrLn $ map (shift (-n)) preimage
      exitSuccess
    Nothing -> putStrLn "Must enter an integer; please try again."

-- matchSize resizes the first string argument to match the second's length using
-- truncation to shrink and duplication to grow.
matchSize :: String -> String -> String
matchSize cs target = go cs "" $ length target
  where
    sz = length cs
    go old acc count
      | count == 0 = acc
      | count < sz = acc ++ take count old
      | otherwise  = go old (acc ++ old) (count - sz)

vigenere :: IO ()
vigenere = do
  hSetBuffering stdout NoBuffering
  putStr "Enter key: "
  ks <- getLine
  putStr "Enter preimage: "
  ps <- getLine
  putStrLn $ zipWith f (matchSize ks ps) ps
    where f = shift . fst . let2Int 

unvigenere :: IO ()
unvigenere = do
  hSetBuffering stdout NoBuffering
  putStr "Enter key: "
  ks <- getLine
  putStr "Enter preimage: "
  ps <- getLine
  putStrLn $ zipWith f (matchSize ks ps) ps
    where f = shift . negate . fst . let2Int
