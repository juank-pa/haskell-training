module Exercises where

import Data.Char

-- Data.Char
-- 1. Char -> Bool -- for both
-- 2. isUpper
filterUpper :: String -> String
filterUpper s = filter isUpper s

-- 3.
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : xs

-- 4. We could have used map but the exercise states we use recursion
uppercase :: String -> String
uppercase ""     = ""
uppercase (x:xs) = toUpper x : uppercase xs

-- 5. I used Maybe to prevent a partial function and pattern matching instead of head
firstCapitalized :: String -> Maybe Char
firstCapitalized ""    = Nothing
firstCapitalized (x:_) = Just (toUpper x)

-- if you want to use head as proposed by the exercise (partial function)
firstCapitalizedPartial :: String -> Char
firstCapitalizedPartial s = toUpper (head s)

-- 6. Safe version
firstCapitalized' :: String -> Maybe Char
firstCapitalized' "" = Nothing
firstCapitalized' xs = (Just . toUpper . head) xs

-- if you want to use head as proposed by the exercise (partial function)
-- composed
firstCapitalizedPartial' :: String -> Char
firstCapitalizedPartial' xs = (toUpper . head) xs

-- pointfree
firstCapitalizedPartial'' :: String -> Char
firstCapitalizedPartial'' = toUpper . head


-- Ciphers
-- see Cipher.hs
