module Main where

import Test.Hspec
import Hangman

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    let toGuess = "boyma"
    let initBoard   = ((map .) const) Nothing toGuess
    let initGuessed = ""
    let initCount = 0
    let initPuzzle = (Puzzle toGuess initBoard initGuessed initCount)
    it "should increase count on a wrong guess" $ do
      case fillInCharacter initPuzzle 'j' of
        (Puzzle _ _ _ n) -> n `shouldBe` 1
    it "should not increase count on a correct guess" $ do
      case fillInCharacter initPuzzle 'b' of
        (Puzzle _ _ _ n) -> n `shouldBe` 0
    it "should update board on a correct guess" $ do
      case fillInCharacter initPuzzle 'b' of
        (Puzzle _ board _ _) -> board `shouldBe` [Just 'b'] ++ take 4 initBoard

    -- could continue on...
