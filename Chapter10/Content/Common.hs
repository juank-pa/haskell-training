{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter10.Content.Common where

import Prelude hiding (sum,concat,length,product,scanl,foldl,foldr)
import Control.Arrow

{-# ANN module "HLint: ignore Use foldr" #-}
sum :: [Integer] -> Integer
sum []     = 0
sum (x:xs) = x + sum xs

length :: [a] -> Integer
length []     = 0
length (_:xs) = 1 + length xs

product :: [Integer] -> Integer
product []     = 1
product (x:xs) = x * product xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =
  q : (case ls of
         []   -> []
         x:xs -> scanl f (f q x) xs)

-- I have departed slightly from the version proposed by the book
-- by starting the sequence from 0 instead of 1.
-- This has two reasons:
-- a) The fibonacci sequence starts with f0 which is equal to zero.
-- b) The fibonacci recursive approach from chapter 8 already states
--    `fibonacci 0` equals zero (which is right). So I maintain identity,
--    between `fibsN` and chapter eights' `fibonacci` function.
fibs = 0 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- Here I append some other interesting versions of fibonacci sequences.
-- This was part of my own experimentation, to get more understanding.

-- Based on tuples and iterate
{-# ANN module "HLint: ignore Use &&&" #-}
fibs2 :: [Integer]
fibs2 = map fst . iterate (\t -> (snd t, uncurry (+) t)) $ (0,1)

-- Using Control.Arrow.&&& we can simplify `fibs2`
fibs2' :: [Integer]
fibs2' = map fst . iterate (snd &&& uncurry (+)) $ (0,1)

-- Using zipWith
fibs3 :: [Integer]
fibs3 = 0:1:zipWith (+) fibs (tail fibs)

-- With a helper recursive function
fibs4 :: [Integer]
fibs4 = go 0 1
  where go f1 f2 = f1 : go f2 (f1 + f2)
