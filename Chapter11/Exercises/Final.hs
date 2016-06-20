{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter11.Exercises.Intermission where

import Data.Char

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

------------------
-- Multiple Choice
------------------
-- 1. a) `Weekday` is a type with five data contructors
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
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf l@(x:xs) (y:ys)
  | x == y    = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf l  ys

-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(l:ls) -> (w,toUpper l : ls)) . words

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
