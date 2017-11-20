import Data.List

-- Eq instances --
--
-- Write the Eq instances for the following datatypes:
-- 1.
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = i == i'

-- 2.
data TwoIntegers =
  Two Integer Integer deriving Show

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') =
    x == x' && y == y'

-- 3.
data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x')     = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _                            = False

-- 4.
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') =
    x == x' && y == y'

-- 5.
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-- 6.
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==)                      _ _ = False

-- 7.
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x')     = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _                      = False

-- Will They Work? --

-- Decide if the following code examples will work.
-- If so, what will they reduce?
--
-- 1.
--    max (length [1, 2, 3])
--        (length [8, 9, 10, 11, 12])
--    a: compiles and returns 5
--
-- 2. compare (3 * 4) (4 * 5)
--    a: compiles and returns LT
--
-- 3. compare "Julie" True
--    a: does not compile
--
-- 4. (5 + 3) > (3 + 6)
--    a: compiles and returns False

-- Chapter Exercises --
--
-- Multiple Choice --
--
-- 1. The Eq class: makes equality tests possible.
-- 2. The typeclass Ord: allows any two values to be compared.
-- 3. Suppose the typeclass Ord has an `>` operator. What is
--    the type of `>`: Ord a => a -> a -> Bool
-- 4. In `x = divMod 16 12`: the type of x is a tuple.
-- 5. The typeclass Integral includes: Int and Integer numbers.
--
-- Does it typecheck? --
--
-- 1. Added deriving Show
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. Added deriving Eq
data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                  then Blah
                  else x

-- 3. a) only values of type Mood are acceptable.
--    b) Passing 9 will result in error because
--       it is not of type Mood and cannot be compared
--       to a Mood.
--    c) `Blah > Woot` will not compile because Mood
--
--       does not implement an instance of Ord.
-- 4. Compiles as is
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do? --
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

-- 1. phew = Papu "chases" True
--    a: Does not compile because of missing data constructors
--
-- 2. truth = Papu (Rocks "chomskydoz") (Yeah True)
--    a: compiles!
--
-- 3. equalityForall :: Papu -> Papu -> Bool
--    equalityForall p p' = p == p'
--    a: compiles!
--
-- 4. comparePapus :: Papu -> Papu -> Bool
--    comparePapus :: p p' = p > p'
--    a: Does not compile because Papu does not implement an instance of Ord.
--
-- Match the types --
--
-- Decide if you can substitute the second type for the first
-- 1. a) i :: Num a => a
--       i = 1
--    b) i :: a
-- Cannot substitute because "1" defaults to a Num a => a
-- which is more specific than a parametrially polymorphic
-- type variable.
--
-- 2. a) i :: Float
--       i = 1.0
--    b) i = Num a => a
-- Cannot sub because "1.0" defaults to type Fractional a => a
-- which is a subclass of Num a => a and has additional functionality
-- that Num a => a does not provide.
--
-- 3. a) f :: float
--       f = 1.0
--    b) f :: Fractional a => a
-- Yes, this compiles.
--
-- 4. a) f :: float
--       f = 1.0
--    b) f :: RealFrac a => a
-- Yes, this compiles.
--
-- 5. a) freud :: a -> a
--       freud x = x
--    b) freud :: Ord a => a -> a
--
-- 6. a) freud' :: a -> a
--       freud' x = x
--    b) freud :: Int -> Int
-- Yes, this compiles. It is overly restrictive though.
--
-- 7. a) myX = 1 :: Int
--       sigmund :: Int -> Int
--       sigmund x = myX
--    b) sigmund :: a -> a
-- Does not compile because a parametrically poly type variable
-- must be able to accept any type but sigmund's definition always
-- returns an Int. This is too specific.
--
-- 8. a) myX = 1 :: Int
--       sigmund :: Int -> Int
--       sigmund x = myX
--    b) sigmund ::  Num a => a -> a
-- Does not compiles because sigmund should return a value of type
-- Num a => a but always returns an Int.

-- 9. a) jung :: Ord a => [a] -> a
--       jung xs = head (sort xs)
--    b) jung :: [Int] -> Int
-- Yes, it compiles.
--
-- 10. a) young :: [Char] -> Char
--        young xs = head (sort xs)
--     b) yound :: Ord a => a -> a
-- Yes, it compiles.
--
-- 11. a) mySort :: [Char] -> [Char]
--        mySort = sort
--        signifier :: [Char] -> Char
--        signifier xs = head (mySort xs)
--     b) signifier :: Ord a => [a] -> a
-- Does not compile because xs wiil have Ord a => typeclass constraint
-- but mySort will expect a [Char] typed value as input. The expected
-- value is too specific.
--
-- Type-Kwon-Do Two: Electric Typealoo
--
-- 1. 
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk x2y x y = x2y x == y

-- 2.
arith :: Num b
      => (a -> b)
      -> Integer 
      -> a
      -> b
-- could have used any arithmetic function
arith x2y z x = fromInteger(z) * (x2y x)
