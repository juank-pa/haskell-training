{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Chapter11.Exercises.Cipher
  ( caesar
  , unCaesar
  , vigenere
  , unVigenere
  , vigenere'
  ) where

import Data.Char
import Data.Bool
import Data.List

-- Helper functions

-- For this exercise I use the `isAsciiUpper` and `isAciiLower` functions because
-- the `isUpper` and `isLower` functions take into account all the Unicode range.
-- If a character with a code outside the Ascii range gets "wrapped" between the
-- 26 Ascii alphabet characters (with mod), then it cannot be
-- decoded back later.

-- Helper to convert a Char to an Int by subtracting a base from its
-- ordinal value
chToInt :: Int -> Char -> Int
chToInt = flip ((-) . ord)

-- Helper to convert an Int to an Char after we add a base to the
-- ordinal value
intToCh :: Int -> Int -> Char
intToCh b = chr . (+b)

-- Determines if a character is between the 26 uppercase and
-- 26 lowercase Ascii alphabet character range.
isAsciiLetter :: Char -> Bool
isAsciiLetter ch = isAsciiUpper ch || isAsciiLower ch

-- Shifts Ascii alphabet characters by and specific amount "wrapping" it to
-- the corresponding range.
-- Characters outside of the Acii alphabet range are not shifted.
-- Here I trust `isUpper` because `base` has already filtered the character
shiftLetter :: Int -> Char -> Char
shiftLetter n ch
  | b /= 0    = (intToCh b . (`mod`26) . (+n) . chToInt b) ch
  | otherwise = ch
  where b = base ch

-- The base for uppercase letter is `ord 'A'` and for lowercase letters
-- is `ord 'a'`. For any other letter is zero.
base :: Char -> Int
base ch
  | isAsciiLetter ch = ord . bool 'a' 'A' . isUpper $ ch
  | otherwise        = 0

---------
-- Caesar
---------
fixedShift :: Int
fixedShift = 5

caesar :: String -> String
caesar = map (shiftLetter fixedShift)

unCaesar :: String -> String
unCaesar = map (shiftLetter (negate fixedShift))

-----------
-- Vigenère
-----------
--
-- I have found simpler implementations on the Internet but they lack
-- the Unicode characters management, space management, output space and punctuation
-- recovery, or letter case management.
-- If you are not worried about this implementation details the solution can
-- be rather simple.

-- This function converts a keyword into a list of shift offset values
shiftList :: String -> [Int]
shiftList str = cycle . map (\ch -> chToInt (base ch) ch) $ str

-- This is a more wholemeal solution.
-- This solution does not count spaces to consume keyword letters as the
-- exercise sample output shows.

-- Helper function:
-- The solution uses mainly a foldl' to have an accumulator to keep track
-- of the currently taken keyword characters.
-- Function (Int -> Int) allows modifying the offset prior to shifting.
vigHelper :: (Int -> Int) -> String -> String -> String
vigHelper _ "" s = s
vigHelper f kw s  = unwords . processWords . words $ s
  where
    fstep (ls, r) w = (drop (length w) ls, zipWith (shiftLetter . f) ls w : r)
    processWords    = reverse . snd . foldl' fstep (shiftList kw,[])

vigenere :: String -> String -> String
vigenere = vigHelper id

unVigenere :: String -> String -> String
unVigenere = vigHelper negate

-- This is a recursive solution.
-- The exercise is not clear whether or not punctuation characters should
-- also be ignored when consuming the keyword.
vigenere' :: String -> String -> String
vigenere' "" str = str
vigenere' kw str = go (shiftList kw) str
  where
    go _  "" = ""
    go ws@(s:ss) (x:xs)
      | x /= ' '  = shiftLetter s x : go ss xs
      | otherwise = x : go ws xs
