{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- a) 54; Num a => a
a = (* 9) 6

-- b) (0, "doge"); Num a => (a, [Char])
b = head [(0,"doge"),(1,"kitteh")]

-- c) (0, "doge"); (Integer, [Char])
c = head [(0 :: Integer ,"doge"),(1,"kitteh")]

-- d) False; Bool
d = if False then True else False

-- e) 5; Int
e = length [1,2,3,4,5]

-- f) False; Bool
f = (length [1,2,3,4]) > (length "TACOCAT")
