module Exercises where

import Data.Int
import Data.Char
import Data.List

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
--
-- How Does Your Garden Grow? --
--
-- Given the following:
-- data FlowerType = 
--     Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--   deriving Show
--   
-- type Gardener = String
-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show
--
-- What is the normal form of the Garden type?
type Gardner = String

data Garden =
    Gardenia Gardner
  | Daisy Gardner
  | Rose Gardner
  | Lilac Gardner
  deriving Show

-- Programmers --
--
-- Write a function that creates all possible types of programmers
data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer{ os = x, lang = y} |
                  x <- allOperatingSystems,
                  y <- allLanguages]

data Quantum =
    Yes
  | No
  | Both
  deriving Show

-- convert :: Quantum -> Bool
-- convert Yes  = True
-- convert No   = True
-- convert Both = True

-- convert Yes  = True
-- convert No   = True
-- convert Both = False

-- convert Yes  = True
-- convert No   = False
-- convert Both = True

-- convert Yes  = False
-- convert No   = True
-- convert Both = True

-- convert Yes  = True
-- convert No   = False
-- convert Both = False

-- convert Yes  = False
-- convert No   = True
-- convert Both = False

-- convert Yes  = False
-- convert No   = False
-- convert Both = True

-- convert Yes  = False
-- convert No   = False
-- convert Both = False
--
-- The Quad --
--
-- How many inhabitants?
--
-- 1. eQuad can have 8 types
-- 2. prodQuad can have 16 types
-- 3. funcQuad can have 256 types
-- 4. prodTBool can have 64 types
-- 5. gTwo can have (4 ^ 4) ^ 4 types
-- 6. fTwo can have 65536 types
--
-- Chater Exercises --
-- 1. Weekday is a type with 5 data constructors
-- 2. f :: Weekday -> String
-- 3. Types defined with the data keyword must be capitalized.
-- 4. The function g xs = xs !! (length xs - 1) returns the last element of a list
--
-- Ciphers --
-- Exercise can be found in ../lib/cipher.hs
--
-- As-patterns
--
-- 1. Write a function that returns True if (and only if) all the values in
--    the first list appear in the second list, though they need not be contiguous.
isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf xs'@(x:xs) ys'@(y:ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf xs' ys

-- 2. Split a sentence into words, then tuple each word with the capitalized version
--    of it.
--
-- Call words on the string
-- Call map with [String] -> [(String, String)]
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map transform . words
  where
    transform cs'@(c:cs) = (cs', toUpper c : cs)

-- Language exercises --
--
-- 1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord (c:cs) = toUpper c : cs

-- 2. Write a function that capitalizes sentences in a paragraph.
--
-- Split string based on "."
-- Call map with capitalizeWord
-- Append period to each String.
-- Call concat on the list.
capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . map transform . map capitalizeWord . split
  where
    split cs
      | cs == ""  = []
      | otherwise = hd : split tl
      where
        hd = takeWhile (/= '.') . dropWhile (== '.') . dropWhile (== ' ') $ cs
        tl = dropWhile (== '.') . dropWhile (/= '.') . dropWhile (== '.') $ cs

    transform cs = cs ++ "."

-- Phone exercises --
--
-- 1 Create a data structure that captures the phone layout.
type Digit = Char
type Characters = [Char]
type Presses = Int

data Button = Button Digit Characters
type DaPhone = [Button]

phone :: DaPhone
phone =
  [Button '1' "1",
   Button '2' "abc2",
   Button '3' "def3",
   Button '4' "ghi4",
   Button '5' "jkl5",
   Button '6' "mno6",
   Button '7' "pqrs7",
   Button '8' "tuv8",
   Button '9' "wxyz9",
   Button '0' "+ 0",
   Button '*' "^*",
   Button '#' ".,#"]

-- 2. Convert the following conversations into the keypresses
--    required to express them.
convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps phone c = foldr fold [] phone
  where
    fold (Button digit chars) acc
      | isUpper c    = ('*' :: Digit, 1 :: Presses) : reverseTaps phone (toLower c)
      | elem c chars = [(digit, getIndex . elemIndex c $ chars)]
      | otherwise    = acc

    getIndex (Just i) = (i + 1) :: Presses

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone cs = foldr fold [] cs
  where
    fold c acc = concat [reverseTaps phone c, acc]

keypresses :: [[(Digit, Presses)]]
keypresses = map (cellPhonesDead phone) convo

-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = length

dpm :: [Presses]
dpm = map fingerTaps keypresses

-- 4. What was the most popular letter for each messsage?
-- sort string
-- recurse to find most popular letter
--   - pattern match on cs'@(c:cs)
--   - find # of current character: takeWhile (==c) cs 
--   - check if this number is larger than current max
--   - recurse with dropWhile (==c) and current max
mostPopularTuple :: [Char] -> (Char, Presses)
mostPopularTuple cs = go (sort cs) (' ', 0)
  where
    go [] tup = tup
    go cs'@(c:_) (maxc, maxp)
      | cnt > maxp = go next (c, cnt)
      | otherwise  = go next (maxc, maxp)
      where
        cnt  = length . takeWhile (==c) $ cs'
        next = dropWhile (==c) cs'

mostPopularLetter :: String -> Char
mostPopularLetter = fst . mostPopularTuple

cost :: String -> Presses
cost = fingerTaps . reverseTaps phone . mostPopularLetter

-- 5. What was the most popular letter overall? What was the most popular
--    word?
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = fst . go (" ", 0) . words . unwords
  where
    go tup [] = tup
    go (maxc, maxp) cs'@(c:_)
      | cnt > maxp = go (c, cnt) next
      | otherwise  = go (maxc, maxp) next
      where
        cnt  = length . takeWhile (==c) $ cs'
        next = dropWhile (==c) cs'

-- Hutton's Razor --
data Expr
  = Lit Integer
  | Add Expr Expr

-- 1. Write an eval function which reduces an expression to a final sum.
eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (+) (eval e1) (eval e2)

-- 2. Write a printer for the expresssions.
printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = concat ["(", printExpr e1, " + ", printExpr e2, ")"]
