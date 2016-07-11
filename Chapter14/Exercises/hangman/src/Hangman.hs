module Hangman where

import Control.Monad (forever, when)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList
  = WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

-- A simple solution for the exercise will require to change this value.
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 2)
  print (length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

{-# ANN module "HLint: ignore Use String" #-}
data Puzzle
  = Puzzle String [Maybe Char] [Char]
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (fmap (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) = (`elem` s)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) = (`elem` s)

-- Preferred using `fromMaybe` instead of pattern matching.
-- Added `fromMaybe` to the imports.
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessedChar =
          if wordChar == guessed
          then Just guessed
          else guessedChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True)  -> do
      putStrLn "You already guessed that\
               \ character, pick something else!"
      return puzzle
    (True, _)  -> do
      putStrLn "This character was in the word,\
               \ filling in the word accordingly"
      return $ fillInCharacter puzzle guess
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again."
      return $ fillInCharacter puzzle guess

-- hlint suggested to use `Control.Monad.when`
-- Syntax looks cleaner that way

-- Here I just changed two lines to solve the exercise.
-- These new code makes the algorithm count only the letters nor found
-- in the word, to determine if the game is over.
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  let guessesNotInWord = filter (`notElem` wordToGuess) guessed
  in when (length guessesNotInWord > 7) $
    do putStrLn   "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $
    do putStrLn "You win!"
       exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                     \ be a single character"
