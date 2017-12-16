module Exercises where

import Control.Monad (forever)
import Data.Char
import Text.Read
import System.IO
import System.Exit (exitSuccess)

-- Intermissionn: Check your understanding
--
-- import qualified Control.Concurrent as CC
-- import qualified Control.Concurrent.MVar as MV
-- import qualified Data.ByteString.Char8 asB
-- import qualified Data.Locator as DL
-- import qualified Data.Time.Clock.POSIX as PSX
-- import qualified Filesystem as FS
-- import qualified Filesystem.Path.CurrentOS as FPC
-- import qualified Network.Info as NI
-- import qualified Safe
-- import Control.Exception (mask, try) import Control.Monad (forever, when) import Data.Bits
-- import Data.Bits.Bitwise (fromListBE) import Data.List.Split (chunksOf)
-- import Database.Blacktip.Types
-- import System.IO.Unsafe (unsafePerformIO)A
--
-- Given the above import list answers:
--
-- 1. What functions are being imported from Control.Monad
--    Answer: forever, when
--
-- 2. Which imports are both qualified and imported in their
--    entirety
--    Answer: All of the qualified imports
--
-- 3. From the name, what doo you suppose importing blacktip's
--    Types module brings in?
--    Answer: datatype declarations
--
-- 4. a) The type signatur refers to three aliased imports.
--       What modules are named in those aliases?
--    Answer: Control.Concurrent, Control.Concurrent.MVar
--            Filesystem.Path.CurrentOS
--
--    b) Which import does FS.writeFile refer to?
--    Answer: Filesystem
--
--    c) Which import did forever come from?
--    Answer: Control.Monad
--
-- Chapter Exercises --
--
-- 1. Can be found in ../../lib/cipher.hs
--
-- 2. Modify palindrome to exit with a false result.
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 3. Modify palindrome to work for sentences.
palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  let line2 = transform line1
  case (line2 == reverse line2) of
    True  -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
  where
    transform cs = [ toLower c | c <- cs, isAlpha c]

-- 4. Given the following code, finish the undefined function.
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  mAge <- getLine
  case readMaybe mAge :: Maybe Integer of
    Just age -> do
      let person = mkPerson name age
      case person of
        Left p -> do
          putStrLn $ "Error: " ++ show p
          return ()
        Right p -> do
          putStrLn $ "Yay! Successfully got a person: " ++ show p
          return ()
    Nothing -> do
      putStrLn "Age must be an integer."
      return ()
