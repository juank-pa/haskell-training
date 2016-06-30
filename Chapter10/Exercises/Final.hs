{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter10.Exercises.Final where

import Data.Bool

---------------------
-- Warm-up and review
---------------------
-- 1.
-- a) All combinations of (stop, vowel, stop)
stops  = "pbtdkg"
vowels = "aeiou"

combs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- b) All combinations starting with p
combs' = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- c) Combinations of nouns and verbs
nouns = ["car", "dog", "mountain", "teacup", "monster", "boat"]
verbs = ["eat", "walk", "run", "swim", "row", "drive"]

sentences = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2. The following function, returns the average number of letters per word in a given sentence
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- 3. Rewrite using fractional division
seekritFunc' :: Fractional a => String -> a
seekritFunc' x =
  (fromIntegral . sum . map length . words) x /
  (fromIntegral . length . words) x

----------------------------------
-- Rewriting functions using folds
----------------------------------
-- Recursive versions in Chapter09/Exercises/Final.hs
-- 1.
-- Redundant and non-point-free version
{-# ANN module "HLint: ignore Redundant if" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}
myOr' :: [Bool] -> Bool
myOr' xs = foldr (\a b -> if a then True else b) False xs
-- better this way
{-# ANN module "HLint: ignore Use or" #-}
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
-- Redundant and non-point-free version
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f xs = foldr (\a b -> if f a then True else b) False xs
-- better this way
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||).f) False

-- 3.
-- Redundant and non-point-free version
myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = foldr (\a b -> if a == x then True else b) False xs
-- better wthis way
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False
-- in terms of any
{-# ANN module "HLint: ignore Use elem" #-}
myElem'' :: Eq a => a -> [a] -> Bool
myElem'' e = any (==e)

-- 4.
-- Non-point-free version
{-# ANN module "HLint: ignore Avoid lambda" #-}
myReverse' :: [a] -> [a]
myReverse' xs = foldl (\b a -> a : b) [] xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
-- Non-point-free version
myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\a b -> f a : b) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6.
-- Non-point-free version
myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f xs = foldr (\a b -> if f a then a : b else b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> bool b (a : b) (f a)) []

-- 7.
-- Non-point-free version
{-# ANN module "HLint: ignore Use concat" #-}
squish' :: [[a]] -> [a]
squish' xs = foldr (++) [] xs

squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
-- Non-point-free version
squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f xs = foldr (\a b -> f a ++ b) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.
-- `squish` in terms of `squishMap`
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\b a -> if f b a == GT then b else a) x xs
myMaximumBy _ _      = error "Empty list"

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\b a -> if f b a == LT then b else a) x xs
myMinimumBy _ _      = error "Empty list"
