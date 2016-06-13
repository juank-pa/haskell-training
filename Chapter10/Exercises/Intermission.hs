module Intermision where

import Data.Time

-- Understanding Folds
-- 1. b) c)
-- 2.
-- foldl (flip (*)) 1 [1,2,3]
-- foldl (flip (*)) (1 * 1) [2,3]
-- foldl (flip (*)) (2 * (1 * 1)) [3]
-- foldl (flip (*)) (3 * (2 * (1 * 1))) []
-- (3 * (2 * (1 * 1)))
-- (3 * (2 * 1))
-- (3 * 2)
-- 6
--
-- 3. c)
-- 4. a)
-- 5.
-- a) foldr (++) [] ["woot", "WOOT", "woot"]
-- b) foldr max '\0' "fear is the little death"
-- c)
-- and [False, True] -- or
-- foldr (&&) True [False, True]
-- d) Can't return a different value than True. Is enough with one True for || to be True.
-- e) foldl (flip ((++) . show)) "" [1..5]
-- f) foldr const 0 [1..5] -- by folding from 'a' would require its result to be a Char but
--    const would return an Integral. So doesn't typecheck.
-- g) foldr const 'a' "tacos" -- same as f) but with flipped data types
-- h) foldl (flip const) 'a' "burritos" -- same as g)  because of the use os foldl
-- i) foldl (flip const) 0 [1..5] -- same as h) but with flipper data types

-- Database Processing
data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 90
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where
    f (DbDate utc) xs = utc:xs
    f _            xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where
    f (DbNumber i) xs = i:xs
    f _            xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max minVal . filterDbDate
  where minVal = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb) xs / (fromIntegral . length . filterDbNumber) xs

-- Scans Exercises
-- 1.
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

firstTwentyFibs :: [Integer]
firstTwentyFibs = take 20 fibs

-- 2.
lessThanHundredFibs :: [Integer]
lessThanHundredFibs = takeWhile (<100) fibs

-- 3. No need for recursion because we can use: scanl (*) 1 [1..]
-- but the exercise expects a recursive approach

factorials :: [Integer]
factorials = 1:scanl (\y z -> (y + z) * (y `div` z)) 1 factorials

-- Reasoning:
-- If we want to get the factorial list recursively, we don't have access to the next
-- factorial input or output values, but to the two previous factorial outputs instead.
--
-- [..., z, y, x!] -- We don't have access to x or x!, but we have access to y and z
--             ^
--             next required number
--
-- The previous can be rewritten as:
--
-- [..., (x - 2)!, (x - 1)!, x!] -- or
-- [..., (x - 2)!, (x - 2)! * (x - 1), x!]
--
-- Being:
--
-- y = (x - 2)! * (x - 1)
-- z = (x - 2)!
--
-- If we add y and z together we have:
--
-- a)
-- y + z
-- ((x - 2)! * (x - 1)) + (x - 2)! -- and by common factor
-- (x - 2)! * ((x - 1) + 1)
-- (x - 2)! * (x - 1 + 1)
-- (x - 2)! * x
--
-- Now we have the product of all numbers up to x, except for (x - 1), which is missing
-- If we divide y by z we have:
--
-- b)
-- y / z
-- ((x - 2)! * (x - 1)) / (x - 2)! -- and by simplification
-- (x - 1)
--
-- We got our missing value. If we multiply both results a) and b) we get:
--
-- (x - 2)! * (x - 1) * x
-- (x - 1)! * x
-- x!
--
-- We got the desired result, and by following the previous steps, our formula too:
-- (y + z) * (y / z)
