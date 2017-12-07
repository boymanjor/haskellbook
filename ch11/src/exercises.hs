module Exercises where

import Data.Int

-- Dog Types --
--
-- types defined in dogs.hs
--
-- 1. Doggies is a type and data constructor
-- 2. Doggies :: * -> *
-- 3. Doggies String :: *
-- 4. Husky 10 :: Num a => Doggies a
-- 5. Husky (10 :: Integer) :: Doggies Integer
-- 6. Mastiff "Scooby Doo" :: Doggies [Char]
-- 7. DogueDeBordeaux is a type and data constructor
-- 8. DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]
--
-- Vehicles --
--
-- types and exercises in vehicles.hs
--
-- Cardinality --
--
-- Calculate the cardinality of the following datatypes
data PugType = PugData
-- 1. |1|

data Airline =
    PapuAir
  | CataspultsR'Us
  | TakeYourChancesUnited
-- 2. |3|
--
-- 3. What is the cardinality of Int16
--    answers: 65536
--
-- 4. Cardinality of Int is (maxBound :: Int) - (minBound :: Int) + 1
--    Cardinality of Integer is dependent on local memory
--
-- 5. 2 ^ 8 == 256
--
-- For Example --
--
-- 1. MakeExample :: Example, querying the type of Example results in error
--
-- 2. :info gives the data declaration of a type and any typeclass instances
--    that are defined for it
--
-- 3. Addingi a type argument to MakeExample changes the type from a nullary
--    constructor to one which requires a single application of a value matching
--    the type arguments type.
--
-- Logic Goats --
-- Answers can be found in toomany.hs
--
-- Pity the Bool
--
data BigSmall =
    Big Bool
  | Small Bool
-- 1. Given the above data declaration what is the resulting types cardinality.
--    answer: 2 + 2 = 4


data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)
-- 2. Given the above data declaration what is the resulting types cardinality.
--    answer: 256 + 2 = 258
