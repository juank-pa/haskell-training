module Cipher
  ( caesar
  , unCaesar
  ) where

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
-- Here I trust `isUpper` because isAsciiLetter has already filtered the character
shiftLetter :: Int -> Char -> Char
shiftLetter n ch
  | isAsciiLetter ch = (intToCh base . (`mod` 26) . (+n) . chToInt base) ch
  | otherwise        = ch
  -- The base to subtract from, and add to, the character ordinal value
  where base = ord . bool 'a' 'A' . isUpper $ ch

shiftAmount :: Int
shiftAmount = 5

-- cipher
caesar :: String -> String
caesar = map (shiftLetter shiftAmount)

unCaesar :: String -> String
unCaesar = map (shiftLetter (negate shiftAmount))
