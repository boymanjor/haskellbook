module Exercises where

-- Grab Bag --
--
-- 1. Which (two or more) of the following are equivalent?
--    a) mTh x y z = x * y * z
--    b) mTh x y = \z -> x * y * z
--    c) mTh x = \y -> \z -> x * y * z
--    d) mTh = \x -> \y -> \z -> x * y * z
-- a: all of them are equivalent
--
-- 2. The type of mTh (above) is Num a => a -> a -> a-> a.
--    Which is the type of mTh 3?
-- a: Num a => a -> a -> a
--
-- 3. Next, we'll practice writing anonymous labmda syntax.
-- a) Rewrite the f function in the where clause.
-- where f n = n + 1
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- b) Rewrite the following to use anonymous lambda syntax:
-- addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

-- c) Rewrite the following so that it doesn't use anonymous lambda syntax:
-- mflip f = \x -> \y -> y x
mflip f x y = f y x

-- Variety Pack --
--
-- 1. Given the following declarations
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3, True)

-- a) What is the type of k?
--    - k :: (x, y) -> x
-- b) What is the type of k2? Is it the same type as k1 or k3?
--    - k2 :: [Char] and it is not the same as k1 or k3
-- c) Of k1, k2, k3, which will return the number 3 as the result?
--    - k1, k3

-- 2. Fill in the definition of the following function:
f2 :: (a, b, c)
   -> (d, e, f)
   -> ((a, d), (c, f))
f2 (a, b, c) (d, e, f)  = ((a, d), (c, f))

-- Case Practice --
--
-- 1. Return x when it is greater than y
functionC x y =
  case z of
    True -> x
    False -> y
  where z = x > y

-- 2. Add 2 if even. Otherwise return input.
add2IfEven n =
  case x of
    True -> n + 2
    False -> n
  where x = mod n 2 == 0

-- 3.
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Artful Dodgy --
--
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- 1. dodgy 1 0
-- a: 1
--
-- 2. dodgy 1 1
-- a: 11
--
-- 3. dodgy 2 2
-- a: 22
--
-- 4. dodgy 1 2
-- a: 21
--
-- 5. dodgy 2 1
-- a: 12
--
-- 6. oneIsOne 1
-- a: 11
--
-- 7. oneIsOne 2
-- a: 21
--
-- 8. oneIsTwo 1
-- a: 21
--
-- 9. oneIsTwo 2
-- a: 22
--
-- 10. oneIsOne 3
--  a: 31
--
-- 11. oneIsTwo 3
--  a. 23
--
-- Guard Duty --
--
-- 1. If we place an otherwise statement at the beginning of a guard block it
--    it will be the only guard matched and the function will always evaluate
--    to its corresponding expression.
-- 2. Changing the order of the guards will effect function evaluation because
--    the guards only set a minimum value instead of a range, e.g. 96 >= 90 but
--    it is also >= 70 so which ever guard is encountered first will be satisfied.

pal xs
  | xs == reverse xs = True
  | otherwise        = False

-- 3. pal returns True if xs is a palindrome
-- 4. pal accepts arguments of type: Eq a => [a]
-- 5. pal :: Eq a => [a] -> Bool

numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

-- 6. numbers returns an indication of whether its argument is positive, negative or zero
-- 7. numbers accepts arguments of type: Ord a => a
-- 8. numbers :: (Ord a, Num a, Num b) => a -> b
--
-- Chapter Exercises --
--
-- 1. A polymorphic function: may resolve to values of different types depending on inputs
-- 2. Char -> [String]
-- 3. (Ord a, Num a) => a -> Bool
-- 4. Is a higher order function
-- 5. Bool
--
-- Let's write code --
--
-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- a) rewrite using divMod
tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
  where
    xLast = divMod x 10
    d     = snd $ divMod (fst xLast) 10

-- b) both versions have the same type
-- c) write a function that finds the 100s digit
hunsD = snd . (`divMod` 10) . fst . (`divMod` 100)

-- 2.
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y z =
  case z of
    False -> x
    True  -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y z
  | z         = y
  | otherwise = x

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4.
roundTrip :: (Show a, Read a) => a -> a
roundTrip x = read (show x)

-- 5. Write a pointfree version of the roundTrip
freeRoundTrip :: (Show a, Read a) => a -> a
freeRoundTrip = read . show

-- 6. Change the roundTrip type signature and get the function working.
roundTripTwo :: (Show a, Read b) => a -> b
roundTripTwo = read . show
-- random example
f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d), (a,c))
