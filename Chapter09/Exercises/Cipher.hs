module Chapter9.Exercises.Cipher where

import Data.Char
import Data.Bool

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

-- Caesar
fixedShift :: Int
fixedShift = 5

caesar :: String -> String
caesar = map (shiftLetter fixedShift)

unCaesar :: String -> String
unCaesar = map (shiftLetter (negate fixedShift))
