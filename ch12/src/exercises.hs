module Exercises where

-- Chapter Exercises --
--
-- Determine the kinds
--
-- 1. Given id :: a -> a
--    What is the kind of a?
--
--    a :: *
--
-- 2. Given r :: a -> f a
--    What is the kind of a and f?
--
--    a :: *
--    f is a function and does not have kind.
--
-- String processing --
--
-- 1. Write a recursive function named replaceThe which replaces all instances
--    of "the" with "a".
notThe :: String -> Maybe String
notThe cs
  | cs == "the" = Nothing
  | otherwise   = Just cs

replaceThe :: String -> String
replaceThe = unwords . replace . words
  where
    replace [] = []
    replace (w:ws) = case notThe w of
      Nothing -> "a" : replace ws
      Just cs -> cs : replace ws

-- 2. Write a recursve function that takes a text/string, breaks it into words,
--    and counts the number of instances of "the" followed by a vowel-initial word.
isThe :: String -> Bool
isThe = (=="the")

vowelHead :: String -> Bool
vowelHead (c:cs) = elem c "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count 0 . words
  where
    count n [] = n
    count n (w1:[]) = n
    count n (w1:w2:ws)
      | isThe w1 && vowelHead w2 = count (n+1) ws
      | isThe w2                 = count n (w2:ws)
      | otherwise                = count n ws

-- 3. Return the number of letters that are vowels in a word.
isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

getVowels :: String -> String
getVowels = filter isVowel

countVowels :: String -> Integer
countVowels = fromIntegral . length . getVowels

-- Validate word --
--
-- Use the Maybe type to write a function that counts the number
-- of vowels in a string and the number of consonants. If the number
-- of vowels exceeds the number of consonants return  Nothing. Otherwise,
-- return the word.
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

countl :: String -> (Int, Int)
countl cs = (length . getVowels $ cs, (length cs) - (length . getVowels $ cs))

valid :: (Int, Int) -> Bool
valid (x, y)
  | x > y     = False
  | otherwise = True

mkWord :: String -> Maybe Word'
mkWord cs = case valid . countl $ cs of
  True  -> Just $ Word' cs
  False -> Nothing

-- It's onl Natural --
--
-- Write functions that convert values between Natural numbers and Integers.
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0 :: Integer
natToInteger (Succ n) = (+1) $ natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n =
  if n < 0
  then Nothing
  else Just . transform $ n
  where
    transform n
      | n == 0    = Zero
      | otherwise = Succ . transform $ n - 1

-- Small library for Maybe --
--
-- 1. Simple boolean checks for Maybe values.
isJust, isNothing :: Maybe a -> Bool
isJust (Just a) = True
isJust _        = False

isNothing Nothing = True
isNothing _       = False

-- 2. The following is the Maybe catamorphism. You can turn a 
--    Maybe value into anything else with this.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing   = z
mayybee _ f (Just x) = f x

-- 3. In case you just want to provide a fallback value.
fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing  = z
fromMaybe _ (Just x) = x

-- 4. Converting between List and Maybe
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5. For when we want to drop the Nothing values from our list.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
  Nothing   -> catMaybes xs
  (Just x') -> x' : catMaybes xs

-- 6. You'll see this called "sequence" later.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = go $ Just []
  where
    go (Just ms) []     = Just $ reverse ms
    go (Just ms) (x:xs) = case x of
      Just m  -> go (Just (m:ms)) xs
      Nothing -> Nothing

-- Small library for Either --
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left x) xs = x:xs
    f _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right x) xs = x:xs
    f _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where
    f (Left x)  (xs, ys) = (x:xs, ys)
    f (Right y) (xs, ys) = (xs, y:ys)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' _ _         = Nothing

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

eitherMaybe'' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe'' g = either' (const Nothing) (Just . g)
