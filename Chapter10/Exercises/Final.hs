module Exercises where

import Data.Bool

-- Warm-up and review
-- 1.
-- a)
stops  = "pbtdkg"
vowels = "aeiou"

combs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- b)
combs' = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- c)
nouns = ["car", "dog", "mountain", "teacup", "monster", "boat"]
verbs = ["eat", "walk", "run", "swim", "row", "drive"]

sentences = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2. Returns the average number of letters per word in a given sentence
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- 3.
seekritFunc' :: Fractional a => String -> a
seekritFunc' x =
  (fromIntegral . sum . map length . words) x /
  (fromIntegral . length . words) x

-- Rewriting functions using folds
-- Recursive versions in Chapter9/Exercises.hs
-- 1.
myOr' :: [Bool] -> Bool
myOr' xs = foldr (\a b -> if a == True then True else b) False xs

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f xs = foldr (\a b -> if f a == True then True else b) False xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||).f) False

-- 3.
myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = foldr (\a b -> if a == x then True else b) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==x)) False

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' e = any (==e)

-- 4.
myReverse' :: [a] -> [a]
myReverse' xs = foldl (\b a -> a : b) [] xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\a b -> f a : b) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6.
myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f xs = foldr (\a b -> if f a then a : b else b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> bool b (a : b) (f a)) []

-- 7.
squish' :: [[a]] -> [a]
squish' xs = foldr (++) [] xs

squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f xs = foldr (\a b -> f a ++ b) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\b a -> if f b a == GT then b else a) x xs

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\b a -> if f b a == LT then b else a) x xs
