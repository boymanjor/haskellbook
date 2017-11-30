module Exercises where

import Data.Bool
import Data.Char

-- EnumFromTo --
--
-- Write enumFromTo for the following types
--
-- recursive, helper function
eft :: (Ord a, Enum a) => a -> a -> [a]
eft curr end
  | curr > end  = []
  | curr == end = curr : []
  | otherwise   = curr : eft (succ curr) end

eftBool :: Bool -> Bool -> [Bool]
eftBool x y = eft x y

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = eft x y

eftInt :: Int -> Int -> [Int]
eftInt x y = eft x y

-- Thy Fearful Symmetry --
-- 
-- 1. Using takeWhile and dropWhile, write a function that splits
--    a string into a list of Strings separated by the individual words.
--
myWords :: String -> [String]
myWords cs
  | cs == ""  = []
  | otherwise = h cs : myWords (t cs)
  where
    h = takeWhile (/=' ') . dropWhile (==' ')
    t = dropWhile (/=' ') . dropWhile (==' ')

-- 2. Next write a function that takes a string and returns a list of strings,
--    nusing newline separators to break up the string.
sentences :: String
sentences = "Tyger Tyger, burning bright\n"
            ++ "In the forests of the night\n"
            ++ "What immortal hand or eye\n"
            ++ "Could frame thy fearful symmetry?"

myLines :: String -> [String]
myLines cs
  | cs == ""  = []
  | otherwise = h cs : myLines (t cs)
  where
    h = takeWhile (/='\n') . dropWhile (=='\n')
    t = dropWhile (/='\n') . dropWhile (=='\n')

-- 3. Abstract away wet code.
--
parse :: String -> Char -> [String]
parse cs delim
  | cs == ""  = []
  | otherwise = h cs : parse (t cs) delim
  where
    h = takeWhile (/= delim) . dropWhile (== delim)
    t = dropWhile (/= delim) . dropWhile (== delim)

myWords' :: String -> [String]
myWords' cs = parse cs ' '

myLines' :: String -> [String]
myLines' cs = parse cs '\n'

-- Comprehend Thy Lists --
--
mySqr :: Integral a => [a]
mySqr = [x ^ (2 :: Integer) | x <- [1..10]]
-- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- Figure out what the output will be based on mySqr.

first :: Integral a => [a]
first = [x | x <- mySqr, rem x 2 == 0]
-- [4, 16, 36, 64, 100]

second :: Integral a => [(a, a)]
second = [(x, y) | x <- mySqr,
                   y <- mySqr,
                   x < 50, y > 50]
-- [(1, 64), (1, 81), (1, 100) ... (49, 64), (49, 81), (49, 100)]

third :: Integral a => [(a, a)]
third = take 5 [(x, y) | x <- mySqr,
                   y <- mySqr,
                   x < 50, y > 50]
-- [(1,64),(1,81),(1,100),(4,64),(4,81)]
--
-- Square Cube --
--
sqr :: Integral a => [a]
sqr  = [x ^ (2 :: Integer) | x <- [1..5]]

cube :: Integral a => [a]
cube = [y ^ (3 :: Integer) | y <- [1..5]]
--
-- 1. Write an expression that will make tuples of the outputs sqr and cube.
tuple :: Integral a => [(a, a)]
tuple = [(x, y) | x <- sqr, y <- cube]

-- 2. Now alter that expression so that it only sues the x and y values
--    that are less than 50.
tuple' :: Integral a => [(a, a)]
tuple' = [(x, y) | x <- sqr, y <- cube, x < 50, y < 50]

-- 3. Apple another function to that list comprehension to determine how many
--    tuples inhabit your output list.
tupLen :: Int
tupLen = length (tuple' :: [(Integer, Integer)])

-- Bottom Madness --
--
-- Will the following expressions return a value or bottom.
--
-- 1. [x^y | x <- [1..5], y <- [2, undefined]]
--    answer: bottom
--
-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
--    answer: value
--
-- 3. sum [1, 2, undefined]
--    answer: bottom
--
-- 4. length [1, 2, undefined]
--    answer: result
--
-- 5. length $ [1, 2, 3] ++ undefined
--    answer: bottom
--
-- 6. take 1 $ filter even [1, 2, 3, undefined]
--    answer: result
--
-- 7. take 1 $ filter even [1, 3, undefined]
--    answer: bottom
--
-- 8. take 1 $ filter odd [1, 3, undefined]
--    answer: result
--
-- 9. take 2 $ filter odd [1, 3, undefined]
--    answer: result
--
--10. take 3 $ filter odd [1, 3, undefined]
--    answer: bottom
--
-- Decide if the following expressions are in NF, WHNF, or neither.
--
-- 1. [1, 2, 3, 4, 5]
--    answer: NF
--
-- 2. 1 : 2 : 3 : 4 : _
--    answer: neither
--
-- 3. enumFromTo 1 10
--    answer: neither
--
-- 4. length [1, 2, 3, 4, 5]
--    answer: neither
--
-- 5. sum (enumFromTo 1 10)
--    answer: neither
--
-- 6. ['a'..'m'] ++ ['n'..'z']
--    answer: neither
--
-- 7. (_, 'b')
--    answer: WHNF
--
-- More Bottoms --
--
-- 1. Will the following return a value or bottom?
--
--    take 1 $ map (+1) [undefined, 2, 3]
--    answer: no
--
-- 2. Will the following return a value or bottom?
--
--    take 1 $ map (+1) [1, undefined, 3]
--    answer: yes
--
-- 3. Will the following expression return a value?
--
--    take 2 $ map (+1) [1, undefined, 3]
--    answer: no
--
-- 4. What does the following mystery function do? What is its type?
--    Describe it in standard English.
itIsMystery :: String -> [Bool]
itIsMystery xs =
  map (\x -> elem x "aeiou") xs

-- itIsMystery takes a string and returns a list of Bools representing
-- whether each character in the String is a lowercase vowel.
--
-- 5. What is the result of the following funcions:
-- a)
first'' :: Integral a => [a]
first'' = map (^ (2 :: Integer)) [1..10]
-- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- b)
second'' :: Integral a => [a]
second'' = map minimum [[1..10], [10..20], [20..30]]
-- [1, 10, 20]

-- c)
third'' :: Integral a => [a]
third'' = map sum [[1..5], [1..5], [1..5]]
-- [15, 15, 15]
--
-- 6. Writea function that does the same earlier example using bool.
negate3 :: Integral a => [a]
negate3 = map (\x -> bool (x) (-x) (x == 3)) [1..10]

-- Filtering --
--
-- 1. Write a filter function that would give us all the multiples of 3
--    out of a list from 1-30.
filter3s :: Integral a => a -> Bool
filter3s = \x -> rem x 3 == 0

-- 2. Compose the above function with the length function
count3s :: Integral a => [a] -> Int
count3s = length . filter filter3s

-- 3. Write a function that accepts a string and returns
--    a list of the words in the string save articles.
filterArticles :: String -> [String]
filterArticles = filter (\x -> not $ elem x ["the", "a", "an"]) . words

-- Zipping exercises --
-- 1. Implement zip
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) =
  (x, y) : zip' xs ys

-- 2. Immplement zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) =
  f x y : zipWith' f xs ys

-- 3. Rewrite zip' in terms of zipWith'
zip'' :: [a] -> [b] -> [(a, b)]
zip'' xs ys = zipWith' (\x -> \y -> (x, y)) xs ys

-- Chapter Exercises --
--
-- Data.Char --
--
-- 2. Write a function that filters out the uppercase letters of a string
--    using isUpper.
filterUpper :: String -> String
filterUpper = filter (\x -> isUpper x)

-- 3. Write a function that will capitalize the frist letter of a string and return
--    the entire string.
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) =
  toUpper x : xs

-- 4. Change the above to a recursive function that capitalizes the entire string.
capitalize' :: String -> String
capitalize' [] = []
capitalize' (x:xs) =
  toUpper x : capitalize' xs

-- 5. Write a function that capitlizes the first char of string and returns that char.
capHead :: String -> Char
capHead xs = toUpper $ head xs

-- 6. Write capHead as a pointfree function.
capHead' :: String -> Char
capHead' = toUpper . head

-- Ciphers --
-- Answers found in ../../lib/cipher.hs
--
-- Writing your own standard functions
--
-- 1. Write or
or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

-- 2. Write any
any' :: (a -> Bool) -> [a] -> Bool
any' _ []     = False
any' f (x:xs) = f x || any' f xs

-- 3. Write a recursive version of elem, then write a verison using any
elem', elem'' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

elem'' x ys = any (\x' -> x == x') ys

-- 4. Implement reverse.
reverse' :: [a] -> [a]
reverse' xs = go xs []
  where
    go []     ys = ys
    go (x:xs) ys = go xs (x:ys)

-- 5. Implement concat
concat' :: [[a]] -> [a]
concat' []            = []
concat' ([]:xss)      = concat' xss
concat' ((x:xs):xss) = x : concat' (xs:xss)

-- 6. Implement concatMap
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat' . map f

-- 7. Implement concat using concatMap
concat'' :: [[a]] -> [a]
concat'' = concatMap' (\x -> x)

-- 8. Implement maximumBy
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ (x:[]) = x
maximumBy' f (x1:(x2:xs))
  | f x1 x2 == GT = maximumBy' f (x1:xs)
  | otherwise     = maximumBy' f (x2:xs)

-- 9. Implement minimumBy
minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' _ (x:[]) = x
minimumBy' f (x1:(x2:xs))
  | f x1 x2 == LT = minimumBy' f (x1:xs)
  | otherwise     = minimumBy' f (x2:xs)

-- 10. Using the above functions implement minimum and maximum.
maximum' :: (Ord a) => [a] -> a
maximum' = maximumBy' compare

minimum' :: (Ord a) => [a] -> a
minimum' = minimumBy' compare
