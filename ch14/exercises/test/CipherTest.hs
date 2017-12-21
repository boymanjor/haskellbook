module CipherTest where

import Data.Char
import Cipher
import Test.QuickCheck

genString :: Gen String
genString = (arbitrary :: Gen String) `suchThat` (all isAscii)

genNonNeg :: Gen Int
genNonNeg = (arbitrary :: Gen Int) `suchThat` (>=0)

-- caesar :: Int -> String -> String
prop_caesar :: Property
prop_caesar =
  forAll genNonNeg $ \n ->
  forAll genString $ \xs -> (uncaesar n . caesar n $ xs) == xs

-- vigenere :: String -> String -> String
prop_vigenere :: Property
prop_vigenere =
  forAll genString $ \ks ->
  forAll genString $ \ps -> (unvigenere ks . vigenere ks $ ps) == ps

main :: IO ()
main = do
  quickCheck prop_caesar
  quickCheck prop_vigenere
