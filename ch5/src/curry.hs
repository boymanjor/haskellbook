module Curry where

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curried :: Integer
        -> Bool
        -> Integer
curried i b =
  i + (nonsense b)

uncurried :: (Integer, Bool)
          -> Integer
uncurried (i, b) =
  i + (nonsense b)

anonymous :: Integer
          -> Bool
          -> Integer
anonymous = \i b -> i + (nonsense b)

nested :: Integer
       -> Bool
       -> Integer
nested = \i -> \b -> i + (nonsense b)

-- curry turns a 2-arity function into a curried function.
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

-- uncurry turns a curried function into a 2-arity function.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b
