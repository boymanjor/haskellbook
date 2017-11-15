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

-- Heal the Sick --

-- Fix the following code lines.
-- 1. let area x = 3. 14 * (x * x)
-- a: let area x = 3.14 * (x * x)
--
-- 2. let double x = b * 2
-- a: let double x = x * 2
--
-- 3.
--    q:
--      x = 7
--       y = 10
--      f = x + y
--
--    a:
--      x = 7
--      y = 10
--      f = x + y

-- Chapter Exercises --

-- Parenthesize --
-- Parenthesize the following expressions without changing their results:
-- 1. 2 + 2 * 3 - 1
-- a: 2 + (2 * 3) - 1
--
-- 2. (^) 10 $ 1 + 1
-- a: (^) 10 (1 + 1)
--
-- 3. 2 ^ 2 * 4 ^ 5 + 1
-- a: (2 ^ 2) * (4 ^ 5) + 1
--
-- Equivalent expressions --
-- Choose which of the following pairs of exprs will return the same results:
-- 1. a) 1 + 1
--    b) 2
--    yes
--
-- 2. a) 10 ^ 2
--    b) 10 + 9 * 10
--    yes
--
-- 3. a) 400 - 37
--    b) (-) 37 400
--    no
--
-- 4. a) 100 `div` 3
--    b) 100 / 3
--    no
--
-- 5. a) 2 * 5 + 18
--    b) 2 * (5 + 18)
--    no
--
-- More fun with functions --
-- Ordering to allow expressions to be entered into the REPL:
-- z = 7
-- y = z + 8
-- x = y ^ 2
-- waxOn = x * 5
-- 1. What will happen if we enter the following into REPL:
-- q: 10 + waxOn
-- a: 1135
--
-- q: (+10) waxOn
-- a: 1135
--
-- q: (-) 15 waxOn
-- a: -1110
--
-- q: (-) waxOn 15
-- a: 1110
--
-- 2. Re-enter `let triple x = x * 3` into the REPL
--
-- 3. What will happe when we enter the following into REPL:
--    triple waxOn
-- a: First, triple will be applied to waxOn. Its expression will be replaced
--    with `waxOn * 3`. Next waxOn will be evaluated. After its value is reduced
--    to 1125 it will replace waxOn in the triple expression,
--    i.e. triple = 1125 * 3. Finally, triple will be evaluated to equal 3375.
--
-- 4. Rewrite waxOn as an expression with a where clause.
waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

-- 5. Rewrite triple.
triple x = x * 3

-- 6. Add waxOff
waxOff x = triple x
