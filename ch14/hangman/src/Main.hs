module Main where

import Hangman
import Data.Char (toLower)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle $ fmap toLower word
  runGame puzzle
