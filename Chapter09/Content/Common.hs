{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Chapter09.Content.Common where

myTail :: [a] -> [a]
myTail []       = []
myTail (_ : xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail [x]    = Nothing
safeTail (_:xs) = Just xs

-- *Not* identical to the length function in Prelude
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

{-# ANN module "HLint: ignore Use foldr" #-}
mySum :: Num a => [a] -> a
mySum []       = 0
mySum (x : xs) = x + mySum xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pr (x:xs)
  | pr x      = x : filter' pr xs
  | otherwise = filter' pr xs
