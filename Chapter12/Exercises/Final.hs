module Chapter12.Exercises.Final where

import Data.Char
import Data.Function
import Data.List

----------------------
-- Determine the kinds
----------------------
-- 1. id :: a -> a -- Kind of a is *
-- 2. r :: a -> f a -- Kind of a is *, kind of f is * -> *

--------------------
-- String processing
--------------------
--1.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

-- `fromMaybe` returns the underlying data if the value is a `Just`
-- otherwise returns a default value.
-- In this exercise you get the opportunity to create your own `fromMaybe`
-- function so I'm not importing `Data.Maybe`
replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

-- If you do not want to go with importing libraries
replaceThe' :: String -> String
replaceThe' = unwords . map (r . notThe) . words
  where r (Just s) = s
        r Nothing  = "a"

-- There is no need for recursion but if you prefer
replaceThe'' :: String -> String
replaceThe'' = unwords . go . map notThe . words
  where
    go []           = []
    go (Just s':xs) = s' : go xs
    go (Nothing:xs) = "a" : go xs

-- And you could have solved it without using `Maybe`

-- 2.
isVowel :: Char -> Bool
isVowel = (`elem` vowels) . toLower

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . map notThe . words
  where
    go []          = 0
    go (Nothing:Just (ch:_):xs)
      | isVowel ch = 1 + go xs
    go (_:xs)      = go xs

-- 3.
countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

--------------------
-- Validate the word
--------------------
newtype Word'
  = Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

-- In next chapter we'll learn how to access content inside a constructor
-- without constructing and decosntructing values.
mkWord :: String -> Maybe Word'
mkWord = partial hasMoreConsonants . Word'
  where
    hasMoreConsonants = uncurry ((<=) `on` length) . vowelsAndCons
    vowelsAndCons     = partition isVowel . text

text :: Word' -> String
text (Word' t) = t

-- Inspired in `Control.Monad.Plus` library to simplify converting elements
-- on `Maybe` based on a predicate.
partial :: (a -> Bool) -> a -> Maybe a
partial f x
  | f x       = Just x
  | otherwise = Nothing

--------------------
-- It's only Natural
--------------------
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0     = Nothing
  | otherwise = Just $  go n
  where
    go 0  = Zero
    go n' = Succ $ go (n' - 1)

--------------------------
-- Small library for Maybe
--------------------------
-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee y _ _        = y

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe = flip mayybee id

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (:[])

-- 5.
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _        = error "Nothing is not allowed"

{-# ANN module "HLint: ignore Use catMaybes" #-}
catMaybes' :: [Maybe a] -> [a]
catMaybes' = map fromJust . filter isJust

-- Some people prefer not to use `fromJust` because it is partial so

-- Helper function inspired in `Data.Maybe.mapMaybe` which
-- filters items that return `Nothing` when applying the function,
-- and leaves only the results that returns `Just`
-- This will allow me to implementing other ones more easily
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x xs -> mayybee xs (:xs) (f x)) []

catMaybes :: [Maybe a] -> [a]
catMaybes = mapMaybe id

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []          = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just x:xs) = mayybee Nothing (Just . (x:)) $ flipMaybe xs

{-# ANN module "HLint: ignore Use <$>" #-}
-- You could also use `fmap` to map the underlying `Maybe` value.
-- You can keep reducing the function using `foldr`, functors
-- and applicative functors. That is a more advanced topic though ...
flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' []          = Just []
flipMaybe' (Nothing:_) = Nothing
flipMaybe' (Just x:xs) = fmap (x:) $ flipMaybe' xs

-- ... but if you want to get a taste, here you have it.
flipMaybe'' :: [Maybe a] -> Maybe [a]
flipMaybe'' = foldr ((<*>) . ((:) <$>)) (Just [])

---------------------------
-- Small library for Either
---------------------------
-- 1.
-- Now that we have a `Maybe` library at hand let's use it.
-- I originally used pattern matching for this function but once
-- I implemented `either'` later in the exercise I prefered to
-- simplify this one.
leftMaybe :: Either a b -> Maybe a
leftMaybe = either' Just (const Nothing)

-- This version will ultimately use `foldr` through `mapMaybe`
lefts' :: [Either a b] -> [a]
lefts' = mapMaybe leftMaybe

lefts'' :: [Either a b] -> [a]
lefts'' = foldr (\x xs -> mayybee xs (:xs) (leftMaybe x)) []

-- or just with pattern matching
lefts''' :: [Either a b] -> [a]
lefts''' = foldr foldStep []
  where foldStep (Left x) xs = x : xs
        foldStep _ xs        = xs

-- 2.
-- I originally used pattern matching for this function but once
-- I implemented `eitherMaybe''` later in the exercise I prefered to
-- simplify this one.
rightMaybe :: Either a b -> Maybe b
rightMaybe = eitherMaybe'' id

rights' :: [Either a b] -> [b]
rights' = mapMaybe rightMaybe

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs,rights' xs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _         = Nothing

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  x) = f x
either' _ g (Right y) = g y

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

----------
-- Write your own iterate and unfoldr
----------
-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f = mayybee [] (\(a, b) -> a : myUnfoldr f b) . f

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

---------------------------------------
-- Finally something other than a list!
---------------------------------------
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f = mayybee Leaf (\(la,b,ra) -> Node (unfold f la) b (unfold f ra)) . f

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\a -> partial ((<=n) . mid) (a + 1, a,a + 1)) 0
  where mid (_,x,_) = x
