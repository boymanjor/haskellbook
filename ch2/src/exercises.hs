module Exercises where

-- Comprehension Check --

-- 1. Given the following lines of code as they might appear in a source file,
--    how would you change them to use them directly in the REPL?
--
--    q: half x = x / 2
--    a: let half x = x / 2
--
--    q: square x = x * x
--    a: let square x = x * x
--
-- 2. Write one function that has one parameter and works for all the following
--    expression. Be sure to name the function.
--
--    3.14 * (5 * 5)
--    3.14 * (10 * 10)
--    etc.
byPi1 x = x * 3.14

-- 3. There is a value in Prelude called pi. Rewrite your function to use pi instead
--    of 3.14
byPi2 x = x * pi

-- Parenthesis and Association --

-- Decide if the parentheses change the result of the function.
-- 1. a) 8 + 7 * 9
--    b) (8 + 7) * 9
--    yes
--
-- 2. a) perimeter x y = (x * 2) + (y * 2)
--    b) perimeter x y = x * 2 + y * 2
--    no
--
-- 3. a) f x = x / 2 + 9
--    b) f x = x / (2 + 9)
--    yes
