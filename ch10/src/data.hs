module Data where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))]

-- Write a function that filters for DbDate values
-- and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr f [] xs
  where
    f x ys = case x of
      DbDate (UTCTime d t) -> UTCTime d t : ys
      otherwise              -> ys

-- Write a function that filters for DbNumber values and
-- returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr f [] xs
  where
    f x ys = case x of
      DbNumber n -> n : ys
      otherwise  -> ys

-- Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr f (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) xs
  where
    f (DbDate (UTCTime d t)) y =
      case compare (UTCTime d t) y of
        GT        -> (UTCTime d t)
        otherwise -> y
    f _ y = y

-- Write a function that sums all of the DbNumbr values
sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr f 0 xs
  where
    f x y = case x of
      DbNumber n -> n + y
      otherwise  -> y

-- Write a function that gets the average of the 
-- DbNumbr values
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb $ xs) / (fromIntegral . length $ filterDbNumber xs)
