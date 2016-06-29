{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chapter09.Exercises.Final where

import Data.Char
import Data.Bool

------------
-- Data.Char
------------
-- 1. The type of isUpper is: Char -> Bool
--    The type of toUpper is: Char -> Char
-- 2. To filter uppercase letters use isUpper
filterUpper :: String -> String
filterUpper = filter isUpper

-- 3. Capitalize the first letter
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : xs
-- or
capitalize' :: String -> String
capitalize' s = map toUpper (take 1 s) ++ drop 1 s

-- 4. We could have used map but the exercise states we use recursion.
--    The idea here is to illustrate how this function differs from
--    `capitalize` in just the recursive call applied to `xs`
uppercase :: String -> String
uppercase ""     = ""
uppercase (x:xs) = toUpper x : uppercase xs

-- anyway, using map
uppercase' :: String -> String
uppercase' = map toUpper

-- 5. Returns the first letter capitalized using `head` (partial)
firstCapitalizedPartial :: String -> Char
firstCapitalizedPartial s = toUpper (head s)

-- 6. composed and point-free
{-# ANN module "HLint: ignore Eta reduce" #-}
firstCapitalizedPartial' :: String -> Char
firstCapitalizedPartial' s = toUpper . head $ s

firstCapitalizedPartial'' :: String -> Char
firstCapitalizedPartial'' = toUpper . head

-- Safe version
firstCapitalized :: String -> Maybe Char
firstCapitalized ""    = Nothing
firstCapitalized (x:_) = Just (toUpper x)

firstCapitalized' :: String -> Maybe Char
firstCapitalized' ""    = Nothing
firstCapitalized' (x:_) = Just . toUpper $ x

----------
-- Ciphers
----------
-- See Chapter09/Exercises/Cipher.hs

--------------------------------------
-- Writing your own standard functions
--------------------------------------
-- We use recursion here. We'll have the opportunity to play
-- with folds later.
-- 1.
{-# ANN module "HLint: ignore Use foldr" #-}
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- myOr' in terms of myAny
myOr' :: [Bool] -> Bool
myOr' = myAny id

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) = x == e || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (==e)

-- 4.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- optimized version
{-# ANN module "HLint: ignore Use foldl" #-}
myReverse' :: [a] -> [a]
myReverse' = go []
  where go acc []     = acc
        go acc (x:xs) = go (x:acc) xs

-- 5.
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8.
-- Used `bool` function from `Data.Bool` here.
-- personally I think sometimes `bool` looks better than if, guards and cases
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x]    = x
myMaximumBy f (x:xs) = bool mx x (f x mx == GT)
  where mx = myMaximumBy f xs
myMaximumBy _ _      = error "Empty list"

-- 9.
-- You could have abstracted the similarities of this and the previous function.
-- Maybe you can do it as an exercise.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x]    = x
myMinimumBy f (x:xs) = bool mx x (f x mx == LT)
  where mx = myMinimumBy f xs
myMinimumBy _ _      = error "Empty list"

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
