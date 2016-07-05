module Chapter13.Exercises.Final where

import Control.Monad
import System.IO
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

-- See Chapter13/Exercises/hangman

-----------------
-- Modifying code
-----------------
-- 1.
-- See Chapter13/Exercises/Cipher.hs
--
-- 2.
-- Modify to exit on False:
--
-- palindrome :: IO ()
-- palindrome = forever $ do
--   line1 <- getLine
--   case line1 == reverse line1 of
--     True -> putStrLn "It's a palindrome!"
--     False -> putStrLn "Nope!"

{-# ANN module "HLint: ignore Use if" #-}
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case line1 == reverse line1 of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 3.
palindrome2 :: IO ()
palindrome2 = forever $ do
  line1 <- getLine
  case palindrome' line1 of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

palindrome' :: String -> Bool
palindrome' s = sanitized == reverse sanitized
  where sanitized = map toLower . filter isLetter $ s

-- 4.
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)
mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a person name: "
  name <- getLine
  putStr "Enter the person's age: "
  age <- getLine
  case mkPerson name (read age) of
    Right p -> putStrLn ("Yay! Successfully got a person: " ++ show p)
    Left e  -> putStrLn ("Error creating person: " ++ show e)
