module UsingQuickCheckTest where

import UsingQuickCheck
import Test.QuickCheck
import Text.Show.Functions()
import Data.List (sort)
import Data.Char

genPosInt :: Gen Int
genPosInt = (arbitrary :: Gen Int) `suchThat` (>0)

-- 1.
prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = (halfIdentity x) == x

-- 2.
prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort
-- 3.

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

-- 4.
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

-- 5.
prop_quotRem :: Property
prop_quotRem = forAll genPosInt $ \y -> \x -> (quot x y) * y + (rem x y) == x

prop_divMod :: Property
prop_divMod = forAll genPosInt $ \y -> \x -> (div x y) * y + (mod x y) == x

-- 6. (^) is not associative or commutative given the failure of the following tests:
prop_expAssociative :: Property
prop_expAssociative =
  forAll genPosInt $ \x ->
  forAll genPosInt $ \y ->
  forAll genPosInt $ \z -> (x :: Int) ^ (y ^ z)  == (x ^ y) ^ z

prop_expCommutative :: Property
prop_expCommutative =
  forAll genPosInt $ \x ->
  forAll genPosInt $ \y -> x ^ y == y ^ x

-- 7.
prop_reverseIdentity :: Eq a => [a] -> Bool
prop_reverseIdentity x = reverseIdentity x == x

-- 8.
prop_dollar :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
prop_dollar f g x = (f $ g $ x) == (f . g) x

-- 9.
-- foldr (:) /= (++) based on the failure of the below property:
prop_cons :: Eq a => [a] -> [a] -> Bool
prop_cons zs xs = foldr (:) zs xs == (++) zs xs

-- foldr (++) [] == concat based on the passing of the below property:
prop_concat :: Eq a => [[a]] -> Bool
prop_concat xs = foldr (++) [] xs == concat xs

-- 10.
-- The below property is false when length xs < n
-- f n xs = length (take n xs) == n

-- The below property holds:
prop_take :: Property
prop_take =
  forAll genPosInt $ \n ->
  forAll (arbitrary :: Gen [Int]) $ \xs -> length (take n xs) <= n

-- 11.
prop_roundtrip :: (Eq a, Show a, Read a) => a -> Bool
prop_roundtrip x = read . show $ x == x

-- Failure --
square :: Num a => a -> a
square x = x * x

squareIdentity :: Floating a => a -> a
squareIdentity = square . sqrt

-- The above property does not hold because sqrt requires a IEEE 754 floating point number.
-- Floating point arithmetic loses precision in different stages of calculations due to rounding errors.

-- Idempotence --
twice, fourTimes :: (a -> a) -> a -> a
twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs

-- 1.
prop_idem1 :: String -> Bool
prop_idem1 x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)

prop_idem2 :: String -> Bool
prop_idem2 x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

main :: IO ()
main = do
  quickCheck (prop_halfIdentity :: Float -> Bool)
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck (prop_listOrdered :: [Int] -> Bool)
  quickCheck (prop_listOrdered :: [Char] -> Bool)
  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (plusCommutative :: Int -> Int -> Bool)
  quickCheck (multAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (multCommutative :: Int -> Int -> Bool)
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck (prop_reverseIdentity :: [Int] -> Bool)
  quickCheck (prop_dollar :: (Int -> Int) -> (Int -> Int) -> Int -> Bool)
  quickCheck (prop_concat :: [String] -> Bool)
  quickCheck prop_take
  quickCheck (prop_roundtrip :: Int -> Bool)
  quickCheck prop_idem1
  quickCheck prop_idem2
