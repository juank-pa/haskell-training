module Main where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO)

import Data.Maybe (isJust, isNothing)
import Data.List (delete)
import Data.Bool (bool)
import Control.Monad.IO.Class (liftIO)
import System.IO.Silently

import Hangman

-- I preferred properties ratter than simple Hspec specs because
-- properties can tests even more edge cases.
-- Thus:
--   * I used `prop` as a shortcut for `it "..." $ property ...`.
--   * I created various generators to control the input in a granular way
--   * Specs for `handleGuess` required the ability to test IO functions.
--     After googling for a while I came up with the `monadicIO` function
--     which allows calling IO functions inside properties (new concept).
--   * The `handleGuess` function was printing to stdout and breaking
--     the spec messages. I found a `silently` package to prevent this
--     messages from printing to stdout (external library).
--   * I used `liftIO` to convert the result of `handleGuess` from an
--     `IO Puzzle` to a `Puzzle` contained in the right monad, so I can
--     retreive the `Puzzle` value with a binding.
--   * I use `context` here to better describe spec input (new concept).
--   * Specs can be done with plain `Hspec`. Try it if you like!

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    prop "always adds the character to the already guessed list" $
      forAll puzzleWithAnyCharGen $
        \(puzzle, ch) ->
          let guessed = guessedChars puzzle
          in guessedChars (fillInCharacter puzzle ch) == ch : guessed
    context "for a character included in the word" $ do
      prop "all letters are unfilled before the function call and some are filled after" $
        forAll puzzleWithExistingCharGen $
          \(puzzle, ch) ->
            all isNothing (filledInSoFar puzzle)
            && (any isJust . filledInSoFar) (fillInCharacter puzzle ch)
      prop "fills the right letters in the puzzle" $
        forAll puzzleWithExistingCharGen $
          \(puzzle, ch) ->
            let result   = fillInCharacter puzzle ch
                isChar   = map (==ch) (word result)
                isFilled = map (== Just ch) (filledInSoFar result)
            in isChar == isFilled
    context "for a character not included in the word" $
      prop "all letters are unfilled before and after the function call" $
        forAll puzzleWithNonExistingCharGen $
          \(puzzle, ch) ->
            all isNothing (filledInSoFar puzzle)
            && (all isNothing . filledInSoFar) (fillInCharacter puzzle ch)
  describe "handleGuess" $ do
    context "for an already guessed character" $
      prop "leaves the puzzle untouched" $
        forAll puzzleWithGuessedCharGen $
          \(puzzle, char) -> monadicIO $ do
            result <- liftIO . silence $ handleGuess puzzle char
            assert (result == puzzle)
    context "for an not yet guessed character" $
      prop "the puzzle is updated accordingly" $
        forAll puzzleWithNonGuessedCharGen $
          \(puzzle, char) -> monadicIO $ do
            result <- liftIO . silence $ handleGuess puzzle char
            assert (result /= puzzle)

{-# ANN module "HLint: ignore Use String" #-}
letterRange :: [Char]
letterRange = ['a'..'z']

-- Puzzle and char that has not been guessed yet
puzzleWithNonGuessedCharGen :: Gen (Puzzle, Char)
puzzleWithNonGuessedCharGen = do
  puzzle <- puzzleGen True
  let
    guessed = guessedChars puzzle
    charElems = foldr delete letterRange guessed
  char <- elements charElems
  return (puzzle, char)

-- Puzzle and char that has not been already guessed
puzzleWithGuessedCharGen :: Gen (Puzzle, Char)
puzzleWithGuessedCharGen = do
  puzzle <- puzzleGen True
  char <- elements (guessedChars puzzle)
  return (puzzle, char)

-- Puzzle and char that isn't contained in the word
puzzleWithNonExistingCharGen :: Gen (Puzzle, Char)
puzzleWithNonExistingCharGen = do
  puzzle <- puzzleGen False
  let
    word' = word puzzle
    charElems = foldr delete letterRange word'
  char <- elements charElems
  return (puzzle, char)

-- Puzzle and char that is contained in the word
puzzleWithExistingCharGen :: Gen (Puzzle, Char)
puzzleWithExistingCharGen = do
  puzzle <- puzzleGen False
  char   <- elements (word puzzle)
  return (puzzle, char)

-- Puzzle and any char
puzzleWithAnyCharGen :: Gen (Puzzle, Char)
puzzleWithAnyCharGen = oneof [ puzzleWithNonExistingCharGen
                             , puzzleWithExistingCharGen ]

-- Puzzle generator.
-- The Bool parameter controls if the already-guessed list is
-- forced to have 2-3 characters (`True`) or 0-3 characters (`False`).
puzzleGen :: Bool -> Gen Puzzle
puzzleGen forceGuessed = do
  word'   <- wordGen 5 9
  guessed <- wordGen (bool 0 2 forceGuessed) 3
  return $ Puzzle word' (map (const Nothing) word') guessed

-- Generates a word of length between the two `Int` parameters.
wordGen :: Int -> Int -> Gen String
wordGen mn mx = do
  k <- choose (mn,mx) :: Gen Int
  sequence [ letterGen | _ <- [1..k] ]

-- Generates a character between 'a' and 'z'
letterGen :: Gen Char
letterGen = elements letterRange

-- Helper functions to retrieve the different puzzle components
guessedChars :: Puzzle -> String
guessedChars (Puzzle _ _ gs) = gs

filledInSoFar :: Puzzle -> [Maybe Char]
filledInSoFar (Puzzle _ fi _) = fi

word :: Puzzle -> String
word (Puzzle w _ _) = w
