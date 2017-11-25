module Exercises where

-- Intermission: Exercise --
--
-- applyTimes 5 (+1) 5
-- (+1) (applyTimes 4 (+1) 5)
-- (+1) ((+1) (applyTimes 3 (+1) 5))
-- (+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))
-- (+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))
-- (+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))
-- (+1) ((+1) ((+1) ((+1) ((+1) 5))))
-- (+1) ((+1) ((+1) ((+1) 6)))
-- (+1) ((+1) ((+1) 7))
-- (+1) ((+1) 8)
-- (+1) 9
-- 10
--
-- Chapter Exercises --
--
-- 1. What is the type of [[True, False], [True, True], [False, True]]?
--
--    a) Bool
--    b) mostly True
--    c) [a]
--    d) [[Bool]]
--
--    asnwer: d
--
-- 2. Which of the following has the same type as [[True, False], [True, True], [False, True]]?
--
--    a) [(True, False), (True, True), (False, True)]
--    b) [[3 == 3], [6 > 5], [3 < 4]]
--    c) [3 == 3, 6 > 5, 3 < 4]
--    d) ["Bool", "more Bool", "Booly Bool!"]
--
--    answer: b
--
-- 3. For the following function:
--
func :: [a] -> [a] -> [a]
func x y = x ++ y
--
--    which of the following is true?
--
--    a) x and y must be of the same type
--    b) x and y must both be lists
--    c) if x is a String then y must be a String
--    d) all of the above
--
--    answer: all of the above
--
-- 4. For the func code above, which is a valid application of func to both of its arguments?
--
--    a) func "Hello World"
--    b) func "Hello" "World"
--    c) [1, 2, 3] "a, b, c"
--    d) func ["Hello", "World"]
--
--    answer: b
--
-- Reviewing currying --
--
-- Given the following definitions, what value results from the further applications.

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. appedCatty "woohoo!"
--    answer: "woops mrow woohoo!"
--
-- 2. frappe "1"
--    answer: "1 mrow haha"
--
-- 3. frappe (apendCatty "2")
--    answer: "woops mrow 2 mrow haha"
--
-- 4. appedCatty (frappe "blue")
--    answer: "woops mrow blue mrow haha"
--
-- 5. cattyConny (frappe "pink")
--               (cattyConny "green" (appendCatty "blue"))
--    answer: "pink mrow haha mrow green mrow woops mrow blue"
--
-- 6. cattyConny (flippy "Pugs" "are") "awesome"
--    answer: "are mrow Pugs mrow awesome"
--
-- Recursion --
--
-- 1. Write out the steps of reducing dividedBy 15 2 to its
--    final answer according to the Haskell code.
--
--    dividedBy 15 2 =
--      go 15 2 0
--        | go (15 - 2) 2 (0 + 1)
--      go 13 2 1
--        | go (13 - 2) 2 (1 + 1)
--      go 11 2 2
--        | go (11 - 2) 2 (2 + 1)
--      go 9 2 3
--        | go (9 - 2) 2 (3 + 1)
--      go 7 2 4
--        | go (7 - 2) 2 (4 + 1)
--      go 5 2 5
--        | go (5 - 2) 2 (5 + 1)
--      go 3 2 6
--        | go (3 - 1) 2 (6 + 1)
--      go 1 2 7
--        | (7, 1)

-- 2. Write a function that sums the integers from 1 to n.
sum' :: (Eq a, Num a) => a -> a
sum' x = go 0 x
    where go total count
            | count == 0 = total
            | otherwise  = go (count + total) (count - 1)

-- 3. Write a function that multiples two integral numbers using recursive
--    summation. The type should be (Integral a) => a -> a -> a.
product :: Integral a => a -> a -> a
product x y = go x y 0
  where go val count total
          | count == 0 = total
          | otherwise  = go val (count - 1) (val + total)

-- Fixing dividedBy --
-- Answer found in division.hs
--
-- McCarthy 91 function --
mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ (n + 11)

-- Numbers into words --
-- Answer found in numbers.hs
