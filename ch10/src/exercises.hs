module Exercises where

import Data.List

-- Understanding Folds --
-- 1. foldr (*) 1 [1..5] will return the same as:
--    a) flip (*) 1 [1..5]
--    b) foldl (flip (*)) 1 [1..5]
--    ---------------------
--    c) foldl (*) 1 [1..5]
--    ---------------------
--
-- 2. Write out the evaluation steps for foldl (flip(*)) 1 [1..3]
--
-- foldl (flip (*)) 1 [1..3]
-- f ~ flip (*), z ~ 1
--
-- foldl f z [1..3]
-- foldl f (f z 1) [2..3]
-- foldl f (f (f z 1) 2) [3]
-- foldl f (f (f (f z 1) 2) 3) []
-- f (f (f z 1) 2) 3
-- f (f (f 1 1) 2) 3
-- f (f 1 2) 3
-- f 2 3
-- 6
--
-- 3. One difference between foldr and foldl is:
--    a) foldr, but not foldl, traverses the spine of a list from right to left
--    b) foldr, but not fold, always forces the rest of the fold
--    ------------------------------------------------
--    c) foldr, but not foldl, associates to the right
--    ------------------------------------------------
--    d) foldr, but not foldl, is recursive
--
-- 4. Folds are catamorphisms, which means they are generally used to
--    -------------------
--    a) reduce structure
--    -------------------
--    b) expand structure
--    c) render you catatonic
--    d) generate infinite data structures
--
-- 5. The following are simple folds very similar to what you've already
--    seen, but each has at least one error. Please fix them and test in
--    your REPL:
--
-- a) foldr (++) ["woot", "WOOT", "woot"]
--    foldr (++) "" ["woot", "WOOT", "woot"]

-- b) foldr max [] "fear is the little death"
--    foldr max ' ' "fear is the little death"

-- c) foldr and True [False, True]
--    foldr (&&) True [False, True]

-- d) foldr (||) True [False, True]
--    foldr (||) False [False, True]

-- e) foldl ((++) . show) "" [1..5]
--    foldr ((++) . show) "" [1..5]

-- f) foldr const 'a' [1..5]
--
--    foldr const 'a' [1..5]
--    f ~ const, z ~ 'a'
--    foldr f z [1..5]
--    f 1 (foldr f z [2..5])
--    f 1 (f 2 (foldr f z [3..5]))
--    f 1 (f 2 (f 3 (foldr f z [4..5])))
--    f 1 (f 2 (f 3 (f 4 (foldr f z [5..5]))))
--    f 1 (f 2 (f 3 (f 4 (f 5 (foldr f z [])))))
--    f 1 (f 2 (f 3 (f 4 (f 5 'a'))))
--    f 1 (f 2 (f 3 (f 4 5)))
--    f 1 (f 2 (f 3 4))
--    f 1 (f 2 3)
--    f 1 2
--    1 :: a but we want something of type b
--
--    foldr (flip const) 'a' [1..5] 

-- g) foldr const 0 "tacos"
--    foldr (flip const) 0 "tacos"
--
-- h) foldl (flip const) 0 "burritos"
--    foldl const 0 "burritos"
--
-- i) foldl (flip const) 'z' [1..5]
--    foldl const 'z' [1..5]
--
--  Data Processing --
--  Answers found in ./data.hs
--
--  Scan Exercises --
--
-- 1. Modify fibs (using scanl) to only return the first
--    20 Fibonacci numbers.
fibs20 = take 20 $ 1 : scanl (+) 1 fibs20

-- 2. Modify fibs to return the Fibonacci numbers that
--    are less than 100.
fibsU100 = takeWhile (<100) $ 1 : scanl (+) 1 fibsU100

-- 3. Write factorial using scan.
factorial n = take n $ 1 : scanl (*) 2 [3..]

-- Chapter Exercises --
--
-- 1. Given the following sets of constants and vowels:

stops  = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that makes all possible 3-tuple
-- combinations of the form stop-vowel-stop, given
-- the below string inputs.
tuples = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- b) Modify the above function so that it only returns
--    combinations beginning with a p.
tuples' = [(x,y,z) |
           x <- stops,
           y <- vowels,
           z <- stops, x == 'p']

-- c) Now make a list of nouns and verbs and repeat the
--    process.
nouns = ["cat", "dog", "man", "woman", "car"]
verbs = ["loves", "fights", "hates", "likes", "drives"]
tuples'' = [concat [x," ", y, " ", z] | x <- nouns, y <- verbs, z <- nouns]

-- 2. Figure out what the following function does.
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
-- This above function finds the average length of
-- the words in a given sentence.
--
-- 3. Rewrite the above function using fractional
--    division.
seekritFunc' x =
  (/) (sum (map genericLength (words x)))
      (genericLength (words x))

-- 1. Rewrite or
or' :: [Bool] -> Bool
or' = foldr (||) False

-- 2. Rewrite any
any' :: (a -> Bool) -> [a] -> Bool
-- any' f xs = (foldr (||) False) ((map f) xs)
-- any' f xs = foldr (||) False $ (map f) xs
-- any' f = \xs -> foldr (||) False . (map f) $ xs
-- any' f = foldr (||) False . (map f)
-- any' f = (.) (foldr (||) False) (map f)
-- any' f = (.) (foldr (||) False) $ map f
-- any' = \f -> (.) (foldr (||) False) . map $ f
-- any' = (.) (foldr (||) False) . map
-- answer is the blackbird of the fold and a map
any' = (foldr (||) False .) . map

-- 3. Rewrite elem twice, once with fold and once
--    with any
elem', elem'' :: Eq a => a -> [a] -> Bool
-- elem' x = foldr (\x' z -> x' == x || z) False
-- elem' x = (foldr (==) False) . (map (==x))
-- elem' x = (.) (foldr (==) False) . map $ (==) x
-- elem' x = (.) (foldr (==) False) . map $ (==) x
-- elem' = (.) (foldr (==) False) . map . (==)
elem'  = (foldr (||) False .) . map . (==)
elem'' = any . (==)

-- 4. Implement reverse.
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- 5. Write map.
map' :: (a -> b) -> [a] -> [b]
-- map' f xs = foldr (\x ys -> f x : ys) [] xs
-- map' f = foldr (\x ys -> f x : ys) []
-- map' f = foldr (\x ys -> (:) (f x) ys) []
-- map' f = foldr (\x -> (:) (f x) ) []
-- map' f = foldr (\x -> (:) (f x) ) []
-- map' f = foldr (\x -> (:) $ f x ) []
-- map' f = foldr (\x -> (:) . f $ x ) []
-- map' f = flip foldr [] (\x -> (:) . f $ x )
-- map' f = flip foldr [] ((:) . f )
-- map' f = flip foldr [] ((:) . f)
-- map' f = flip foldr [] . (.) (:) $ f
-- map' = flip foldr [] . (.) (:)
map' = flip foldr [] . ((:) .)

-- 6. Write filter.
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x ys -> if f x then x : ys else ys) []

-- 7. Write concat
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- 8. Write concatMap
concatMap' :: (a -> [b]) -> [a] -> [b]
-- concatMap' f xs = concat' (map' f xs)
-- concatMap' f xs = concat' $ map' f xs
-- concatMap' f xs = concat' . map' f $ xs
-- concatMap' f = concat' . map' f
-- concatMap' f = (.) concat' (map' f)
-- concatMap' f = (.) concat' $ map' f
-- concatMap' f = (.) concat' . map' $ f
-- concatMap' = (.) concat' . map'
-- Another blackbird!!!!
concatMap' = (concat' .) . map'

-- 9. Write concat using concatMap'
concat'' :: [[a]] -> [a]
concat'' =  concatMap' id

-- 10. Write maximumBy
maximumBy' :: (a -> a -> Ordering)
          -> [a]
          -> a
maximumBy' f = foldr1 g
  where
    g x y = case (f x y) of
      GT -> x
      _  -> y

-- 11. Write minimumBy
minimumBy' :: (a -> a -> Ordering)
          -> [a]
          -> a
minimumBy' f = foldr1 g
  where
    g x y = case (f x y) of
      LT -> x
      _  -> y
