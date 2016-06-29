module Wordnumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = go n []
  where go 0 xs = xs
        go x xs = go d (m:xs) where (d,m) = divMod x 10

-- Used intercalate here which is the same as concat . intersperse. Recommended by hlint
wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
