module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords ::IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in  l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle word discovered guessed count) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ "\nGuessed so far: " ++ guessed
    ++ "\n# of misses left: " ++ show (length word - count)

freshPuzzle :: String -> Puzzle
freshPuzzle cs = Puzzle cs (map (const Nothing) cs) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle cs _ _ _) c = elem c cs

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ cs _) c = elem c cs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c)  = c

fillInCharacter :: Puzzle -> Char -> Bool -> Puzzle
fillInCharacter (Puzzle word filledIn s count) c goodGuess =
  Puzzle word newFilledIn (c : s) newCount
  where
    zipper guessed wordChar guessedChar =
      if wordChar == guessed
      then Just wordChar
      else guessedChar

    newFilledIn = zipWith (zipper c) word filledIn

    newCount =
      if goodGuess
      then count
      else count + 1

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ somthing else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return $ fillInCharacter puzzle guess True
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return $ fillInCharacter puzzle guess False

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledIn guessed count) =
  if count == (length wordToGuess) && (not $ all isJust filledIn) then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledIn _ _) =
  if all isJust filledIn then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle $ fmap toLower word
  runGame puzzle
