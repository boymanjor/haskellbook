module Addition where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
              go (n - d) d (count + 1)

mult :: (Eq a, Num a) => a -> a -> a
mult x y = go x y 0
  where go v c acc
          | c == 0    = acc
          | otherwise = go v (c - 1) (acc + v)

oneToThree :: Gen Int
oneToThree = elements [1, 2, 3]

weightedOneToThree :: Gen Int
weightedOneToThree = elements [1, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genChar :: Gen Char
genChar = elements ['a'..'z']

genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      1 + 1 > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always\
       \ greater than x" $ do
        property $ \x -> x + 1 > (x :: Int) -- must inclue a type assertion

  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 dividedBy 5 is\
       \ 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "Multiplication" $ do
    it "5 multiplied by 3 is 15" $ do
      mult 5 3 `shouldBe` 15
