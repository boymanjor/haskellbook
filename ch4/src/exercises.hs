module Exercises where

-- Mood Swing --

-- Given the following data declaration, answer the following questions:
data Mood = Blah | Woot deriving Show

-- 1. What is the type constructor, or name of this type?
-- a: Mood
--
-- 2. If the function requires a Mood value, what are the acceptable values.
-- a: Blah or Woot
--
-- 3. If we want to write a function that works like `not` to change someone's
--    mood, what is wrong about the following type signature:
--    changeMood :: Mood -> Woot
--    a: Woot is not a type constructor and does not belong at the type level.
--       The type signature should be: changeMood :: Mood -> Mood
--
-- 4. Write the above mentioned function
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah
--
-- 5. Completed by including the above code in the source.

-- Find the Mistakes --
--
-- Fix the mistakes in the following lines of code:
-- 1. q: not True && true
--    a: not True && True
--
-- 2. q: not (x = 6)
--    a: not (x == 6)
--
-- 3. q: (1 * 2) > 5
--    a: correct
--
-- 4. q: [Merry] > [Happy]
--    a: "Merry" > "Happy"
--
-- 5. q: [1, 2, 3] ++ "look at me!"
--    a: concat ["1", "2", "3", " look at me!"]

-- Chapter Exercises --

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- `length` is a function that takes a list and returns a result that
--  tells how many items are in the list.
--
-- 1. Given the above definition what would the type signature of length be?
--    a: length :: [a] -> Int
--
-- 2. What are the results of the following expressions?
--    a) length [1, 2, 3, 4, 5] == 5
--    b) length [(1, 2), (2, 3), (3, 4)] == 3
--    c) length allAwesome == 2
--    d) length (concat allAwesome) == 5
--
-- 3. Given what we know about numeric types which of the following expression
--    will return an error and why?
--
--    a) 6 / 3
--    b) 6 / length [1, 2, 3]
--    a: b) will error because (/) accepts arguments which implement instances of
--       the Fractional type class and `length` will return an `Int`
--
-- 4. How can you fix the above code using a different division operator?
--    a: 6 `div` length [1, 2, 3]
--
-- 5. What is the type of the expression `2 + 3 == 5`? What would we expect as
--    a result?
--    type: Bool
--    result: True
--
-- 6. What is the type and expected result of the following:
--    let x = 5
--    x + 3 == 5
--    type: Bool
--    result: False
--
-- 7. Which of the following will/not compile? If it compiles, what is the result?
--    q: length allAwesome == 2
--    a: True
--
--    q: length [1, 'a', 3, 'b']
--    a: error
--
--    q: length allAwesome + length awesome
--    a: 5
--
--    q: (8 == 8) && ('b' < 'a')
--    a: False
--
--    q: (8 == 8) && 9
--    a: error
--
-- 8. Write a function that tells whether or not a given string is a palindrom?
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- 9. Write a function to return the absolute value of a number using if-then-else.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

-- 10. Fill in the definition of the following function, using `fst` and `snd`:
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b =
  ((snd a, snd b), (fst a, fst b))

-- Correcting syntax --
-- 1. Should add 1 to the length of a string argument.
--    x = (+)
--    F xs = w 'x' 1
--         where w = length xs
x = (+)
f2 xs = w `x` 1
  where w = length xs

-- 2. Should be the identity function.
--   \X = x
id = \x -> x

-- 3. Should return 1 from the value (1, 2)
--   f (a b) = A
f3 (a, b) = a

-- Match the function names to their types:
-- 1. show :: Show a => a -> String
-- 2. (==) :: Eq => a -> a -> Bool
-- 3. fst :: (a, b) -> a
-- 4. (+) :: Num a => a -> a -> a
