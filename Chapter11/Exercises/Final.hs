{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter11.Exercises.Final where

import Data.Char
import Data.Bool

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

------------------
-- Multiple Choice
------------------
-- 1. a) We can say `Weekday` is a type with five data contructors
-- 2. The type of:
f Friday = "Miller Time" -- is
--    c) f :: Weekday -> String
-- 3. Types defined with data:
--    b) Must begin with a capital letter
-- 4.
g xs = xs !! (length xs - 1)
--    c) delivers the final element of xs

----------
-- Ciphers
----------
-- See Chapter11/Exercises/Cipher.hs

--------------
-- As-patterns
--------------
-- 1.
-- This not only checks the letter on the first string are contained in the second.
-- But also if they are in the same order, even if intercalated with other letters.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf l@(x:xs) (y:ys)
  | x == y    = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf l  ys

-- 2.
-- You can use the definition on the next section but you would beat the
-- purpose of the as-patterns exercise.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalize . words
  where
    capitalize []       = ([],[])
    capitalize w@(x:xs) = (w,toUpper x:xs)

---------------------
-- Language exercises
---------------------
-- 1.
capitalizeWord :: String -> String
capitalizeWord ""     = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = go True
  where
    go _ "" = ""
    go cap (x:xs)
      | x == '.'          = x : go True xs
      | cap && isLetter x = toUpper x : go False xs
      | otherwise         = x : go cap xs

-----------------
-- Phone exercise
-----------------
-- See Chapter11/Exercises/DaPhone.hs

-----------------
-- Hutton's Razor
-----------------
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit v)   = v
eval (Add l r) = eval l + eval r

printExpr :: Expr -> String
printExpr (Lit v)   = show v
printExpr (Add l r) = printExpr l ++ " + " ++ printExpr r
