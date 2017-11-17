module Exercises where

-- Type Matching --

-- Figure out the type signatures for the following functions.
-- a) not :: Bool -> Bool
-- b) length :: [a] -> Int
-- c) concat :: [[a]] -> [a]
-- d) head :: [a] -> a
-- e) (<) Ord a => a -> a -> Bool

-- Type Arguments --

-- Given a function and its type, discern what type results from
-- partial application of some of the args.
--
-- 1. f :: a -> a -> a -> a
--    x :: Char
--    q: f x
--    a: Char -> Char -> Char
--
-- 2. g :: a -> b -> c -> b
--    q: g 0 'c' "woot"
--    a: Char
--
-- 3. h :: (Num a, Num b) => a -> b -> b
--    q: h 1.0 2
--    a: Num b => b
--
-- 4. h :: (Num a, Num b) => a -> b -> b
--    q: h 1 (5.5 :: Double)
--    a: Double
--
-- 5. jackal :: (Ord a, Eq b) => a -> b -> a
--    q: jackal "keyboard" "has the word jackal in it"
--    a: [Char]
--
-- 6. kessel :: (Ord a, Num b) => a -> b -> a
--    q: kessel 1 2
--    a: Ord a => a
--
-- 7. kessel :: (Ord a, Num b) => a -> b -> a
--    q: kessel 1 (2 :: Integer)
--    a: Ord a => a
--
-- 8. kessel :: (Ord a, Num b) => a -> b -> a
--    q: kessel (1 :: Integer) 2
--    a: Integer

-- Parametricity --

-- 1. Given the type a -> a (identity) attempt to make a function
--    that terminates successfully that does something other than
--    returning the same value.
--    a: this is impossible, because parametrically polymorphic
--       values can only be passed/not to another expression.
--
-- 2. Write the possible imnplementations of a fucntion with type:
--    a -> a -> a
f :: a -> a -> a
f x y = x

f' :: a -> a -> a
f' x y = y

-- 3. Implement a -> b -> b. How many implementations are possible?
--    Does the functions behavior change when the types of a and b
--    change?
--    a: There is only one possible implemntation and it holds true
--       regardless of what types are passed to the function.
g :: a -> b -> b
g x y = y

-- Apply Yourself --

-- Given the following functions. Figure out how the inferred type
-- given the application of the functions.
--
-- 1. (++) :: [a] -> [a] -> [a]
--    q: myConcat x = x ++ " yo"
--    a: [Char] -> [Char]
--
-- 2. (*) :: Num a => a -> a -> a
--    q: myMult x = (x / 3) * 5
--    q: Fractional a => a -> a
--
-- 3. take :: Int -> [a] -> [a]
--    q: myTake x = take x "hey you"
--    a: Int -> [Char]
--
-- 4. (>) :: Ord a => a -> a -> Bool
--    q: myCom x = x > (length [1..10])
--    a: Int -> Bool
--
-- 5. (<) :: Ord a => a -> a -> Bool
--    a: myAlph x = x < 'z'
--    q: Char -> Bool

-- Chapter Exercises --

-- 1. A value of type [a] is: a list whose elements are all of some type a
-- 2. A function of type [[a]] -> [a] could: take a list of strings as an argument
-- 3. A function of type [a] -> Int -> a: returns one element of type a from a list
-- 4. A function of type (a, b) -> a: takes a tuple argument and returns the first value
--
-- Determine the type --
-- Answers found in ./determine.hs
--
-- Does it compile? --
--
-- For each set of expression figure out if the compiler will them. Fix if not.
-- 1.
--   bigNum = (^) 5 $ 10
--   wahoo = bigNum $ 10
-- fix:
--   bigNum = (^) 5
--   wahoo = bigNum $ 10
--
-- 2.
--    x = print
--    y = print "woohoo!"
--    z = x  "hello world!"
-- compiles
--
-- 3.
--    a = (+)
--    b = 5
--    c = b 10
--    d = c 200
-- fix:
--    a = (+)
--    b = 5
--    c = a 10
--    d = c 200
--
-- 4.
--    a = 12 + b
--    b = 10000 * c
-- Will not compile. Need more information.
--
-- Type variable or specific type constructor? --
--
-- Categorize each component of the following type signatures:
-- 1. Answer given
-- 2. f :: zed -> Zed -> Blah
--    0) zed is fully poly
--    1) Zed is concrete
--    2) Blah is concrete
--
-- 3. f :: Enum b => a -> b -> C
--    0) a is fully poly
--    1) b is constrained poly
--    2) C is concrete
--
-- 4. f :: f -> g -> C
--    0) f is fully poly
--    1) g is constrained poly
--    2) C is concrete
--
-- Write a type signature --
--
-- Add type signatures to the following functions:
-- 1.
functionH :: [a] -> a
functionH (x:_) = x

-- 2.
functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given the type, write the function --
--
-- 1.
i :: a -> a
i x = x

-- 2.
c :: a -> b -> a
c x y = x

-- 3. Given alpha equivalence is c'' the same as c above?
-- yes
c'' :: b -> a -> b
c'' x y = x

-- 4.
c' :: a -> b -> b
c' x y = y

-- 5.
r :: [a] -> [a]
r xs = xs

-- 6.
co :: (b -> c) -> (a -> b) -> a -> c
co y2z x2y x = y2z $ x2y x

-- 7.
a :: (a -> c) ->  a -> a
a x2z x = x

-- 8.
a' :: (a -> b) ->  a -> b
a' x2y x = x2y x

-- Fix it --

-- 1. & 2. can be found in ./sing.hs
-- 3. can be found in ./arith3broken.hs
-- 
-- Type-Kwon-Do
-- 1.
f'' :: Int -> String
f'' = undefined

g'' :: String -> Char
g'' = undefined

h'' :: Int -> Char
h'' x = g'' $ f'' x

-- 2.
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X,Y) -> (Z,Z)
xform (x,y) = (xz x, yz y)

-- 4.
munge :: (x -> y)
      -> (y -> (w,z))
      -> x
      -> w
munge x2y y2wz x =
  fst $ y2wz $ x2y x
