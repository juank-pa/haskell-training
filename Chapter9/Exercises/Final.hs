module Chapter9.Exercises.Final where

import Data.Char
import Data.Bool

-- Data.Char
-- 1. Char -> Bool -- for both
-- 2. isUpper
filterUpper :: String -> String
filterUpper = filter isUpper

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
firstCapitalizedPartial' s = (toUpper . head) s

-- pointfree
firstCapitalizedPartial'' :: String -> Char
firstCapitalizedPartial'' = toUpper . head

-- Ciphers
-- see Cipher.hs

-- Writing your own standard functions
-- 1.
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
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x]    = x
myMaximumBy f (x:xs) = bool mx x (f x mx == GT)
  where mx = myMaximumBy f xs
myMaximumBy _ _      = error "Empty list"

-- 9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x]    = x
myMinimumBy f (x:xs) = bool mx x (f x mx == LT)
  where mx = myMinimumBy f xs
myMinimumBy _ _      = error "Empty list"

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
