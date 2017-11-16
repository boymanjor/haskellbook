module Exercises where

-- Scope --

-- 1. This is REPL code. Is `y` in scope for `z`?
--
--    let x = 5
--    let y = 7
--    let z = x * y
--    yes
--
-- 2. This is REPL code. Is `h` in scope for `g`?
--
--    let f = 3
--    let g = 6 * f + h
--    no
--
-- 3. This is source file code. Is everything necessary in scope?
--    area d = pi * (r * r)
--    r = d / 2
--    no, `d` and `r` are both top level definitions that depend on each other.
--
-- 4. This is source file code. Now are `r` and `d` in scope for `area`?
--    area d = pi * (r * r)
--      where r = d / 2
--    yes

-- Syntax Errors --

-- Will the following code compile?
-- 1. ++ [1, 2, 3] [4, 5, 6]
--    no
--    fix: (++) [1, 2, 3] [4, 5, 6]
--
-- 2. '<3' ++ ' Haskell'
--    no
--    fix: "<3" ++ " Haskell"
--
-- 3. concat ["<3", " Haskell"]
--    yes

--  Chapter Exercises --

-- Is the syntax correct?
-- a) concat [[1, 2, 3], [4, 5, 6]]
-- yes
--
-- b) ++ [1, 2, 3] [4, 5, 6]
-- no
-- fix: (++) [1, 2, 3] [4, 5, 6]
--
-- c) (++) "hello" " world"
-- yes
--
-- d) ["hello" ++ " world]
-- no
-- fix: ["hello" ++ " world"]
--
-- e) 4 !! "hello"
-- no
-- fix: "hello" !! 4
--
-- f) (!!) "hello" 4
-- yes
--
-- g) take "4 lovely"
-- no
-- fix: take 4 "lovely"
--
-- h) take 3 "awesome"
-- yes
--
-- 2. Match code with results:
-- concat [[1 * 6], [2 * 6], [3 * 6]] => [6, 12, 18]
-- "rain" ++ drop 2 "elbow" => "rainbow"
-- 10 * head [1, 2, 3] => 10
-- (take 3 "Julie") ++ (tail "yes") => "Jules"
-- concat [tail [1, 2, 3],
--         tail [4, 5, 6],
--         tail [7, 8, 9]] => [2, 3, 5, 6, 8, 9]
--
-- Building functions
--
-- 1. Write expression that take inputs and return outputs.
-- a) Given: "Curry is awesome"
--    Return: "Curry is awesome!"
--    Answer: "Curry is awesome" ++ "!"

-- b) Given: "Curry is awesome!"
--    Return: "y"
--    Answer: take 1 $ drop 4 "Curry is awesome!"

-- c) Given: "Curry is awesome!"
--    Return: "awesome!"
--    Answer: drop 9 "Curry is awesome!"
--
-- 2. Write functions that take inputs and return outputs.
-- a) Given: "Curry is awesome"
--    Return: "Curry is awesome!"
emphasize :: String -> String
emphasize x = x ++ "!"

-- b) Given: "Curry is awesome!"
--    Return: "y"
fifth :: String -> String
fifth x = take 1 $ drop 4 x

-- c) Given: "Curry is awesome!"
--    Return: "awesome!"
drop9 :: String -> String
drop9 x = drop 9 x

-- 3.Write a function of type String -> Char which returns 3rd letter of a String.
thirdLetter :: String -> Char
thirdLetter x = (!!) x 2

-- 4. Change above to use the same string "Curry is awesome" and return nth letter.
nthLetter :: Int -> Char
nthLetter n = (!!) "Curry is awesome!" $ n - 1

-- 5. Write a function that reverses the words in "Curry is awesome"
--    ONLY HAS TO WORL FOR "Curry is awesome"
rvrs :: String -> String
rvrs x =  concat [drop 9 x, take 4 $ drop 5 x, take 5 x]

-- 6. Found in rvrs.hs
