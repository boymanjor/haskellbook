module Exercises where

-- EnumFromTo --
--
-- Write enumFromTo for the following types
--
-- recursive, helper function
eft :: (Ord a, Enum a) => a -> a -> [a]
eft x y = go x y []
  where go curr end list
          | curr > end  = []
          | curr == end = reverse (curr:list)
          | otherwise   = go (succ curr) end (curr:list)

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
myWords x =
  let nextTok = takeWhile (/=' ')
      dropTok = dropWhile (==' ') . dropWhile (/=' ')
      go str list
        | str == "" = reverse list
        | otherwise = go (dropTok str) (nextTok str:list)
  in go x []

-- 2. Next write a function that takes a string and returns a list of strings,
--    nusing newline separators to break up the string.
--    
sentences :: String
sentences = "Tyger Tyger, burning bright\n"
            ++ "In the forests of the night\n"
            ++ "What immortal hand or eye\n"
            ++ "Could frame thy fearful symmetry?"

myLines :: String -> [String]
myLines x =
  let nextTok = takeWhile (/='\n')
      dropTok = dropWhile (=='\n') . dropWhile (/='\n')
      go str list
        | str == "" = reverse list
        | otherwise = go (dropTok str) (nextTok str:list)
  in go x []

-- 3. Abstract away wet code.
--
parse :: String -> Char -> [String]
parse input delim =
  let nextTok = takeWhile (/= delim)
      dropTok = dropWhile (== delim) . dropWhile (/= delim)
      go inp list
        | inp == "" = reverse list
        | otherwise = go (dropTok inp) (nextTok inp:list)
  in go input []

myWords' :: String -> [String]
myWords' x = parse x ' '

myLines' :: String -> [String]
myLines' x = parse x '\n'
