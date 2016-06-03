module Cipher
  ( caesar
  , unCaesar
  ) where

import Data.Char
import Data.Bool

-- helper values
minLower = 'a'
maxLower = 'z'

minUpper = 'A'
maxUpper = 'Z'

-- helper functions

isChar :: Char -> Bool
isChar ch =
     ch >= minLower && ch <= maxLower
  || ch >= minUpper && ch <= maxUpper

shiftLetter :: Int -> Char -> Char
shiftLetter n ch
  | isChar ch = (chr . (+base) . (`mod` 26) . (+n) . subtract base . ord) ch
  | otherwise = ch
  where
    base = (ord . bool minLower minUpper) (isUpper ch)

-- cipher

caesar :: String -> String
caesar = map (shiftLetter 5)

unCaesar :: String -> String
unCaesar = map (shiftLetter (-5))
