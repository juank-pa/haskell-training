module Chapter10.Exercises.Intermision where

import Data.Time

----------------------
-- Understanding Folds
----------------------
-- 1. foldr (*) 1 [1..5] -- returns the same as
--    b) foldl (flip (*)) 1 [1..5]
--    c) foldl (*) 1 [1..5]
-- 2. Evaluate: foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) 1 [1,2,3]
-- foldl (flip (*)) (1 * 1) [2,3]
-- foldl (flip (*)) (2 * (1 * 1)) [3]
-- foldl (flip (*)) (3 * (2 * (1 * 1))) []
-- (3 * (2 * (1 * 1)))
-- (3 * (2 * 1))
-- (3 * 2)
-- 6
--
-- 3. Difference between foldr and foldl
--    c) foldr, but not foldl, associates to the right
-- 4. Folds are catamorphisms, which means they are generally used to
--    a) reduce structure
-- 5. Fix
-- a) foldr (++) ["woot", "WOOT", "woot"]
--    fixed: foldr (++) [] ["woot", "WOOT", "woot"]
-- b) foldr max [] "fear is the little death"
--    fixed: foldr max '\0' "fear is the little death"
-- c) foldr and True [False, True]
--    fixed: and [False, True]
--    or:    foldr (&&) True [False, True]
-- d) foldr (||) True [False, True] -- can return a different answer?
--    Can't return a different value than `True`. Is enough with one `True` for || to be `True`
--    and the initial value is `True`.
-- e) foldl ((++) . show) "" [1..5]
--    fixed: foldl (flip ((++) . sow)) "" [1..5]
-- f) foldr const 'a' [1..5]
--    fixed: foldr const 0 [1..5] -- folding from 'a' would require its result to be a `Char` but
--    const would return an `Integral`. So doesn't typecheck.
-- g) foldr const 0 "tacos"
--    fixed: foldr const 'a' "tacos" -- same as f) but with flipped data types
-- h) foldl (flip const) 0 "burritos"
--    fixed: foldl (flip const) 'a' "burritos" -- same as g) because of the use of foldl
-- i) foldl (flip const) 'z' [1..5]
--    fixed: foldl (flip const) 0 [1..5] -- same as h) but with flipper data types

----------------------
-- Database Processing
----------------------
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
    f (DbDate t) xs = t:xs
    f _          xs = xs

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

------------------
-- Scans Exercises
------------------
-- 1. First 20 fib numbers
-- I have departed slightly from the version proposed by the book
-- by starting the sequence from 0 instead of 1.
-- This has two reasons:
-- a) The fibonacci sequence starts with f0 which is equal to zero
-- b) The fibonacci recursive approach from chapter 8 already states
--    `fibonacci 0` equals zero. So we need to maintain identity.
--    This way `fibsN x = fibs !! x` will return the same values as
--    chapter 8 `fibonacci` function.
fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

firstTwentyFibs :: [Integer]
firstTwentyFibs = take 20 fibs

-- 2.
lessThanHundredFibs :: [Integer]
lessThanHundredFibs = takeWhile (<100) fibs

-- 3. No need for recursion because we can use: scanl (*) 1 [1..]
-- but the exercise expects a recursive approach suggesting `scanl`

-- To get factorials recursively in an easy way, you need to keep track of the
-- current factorial index. A simple approach could be using tuples starting with:
-- (  0,      1)    -- which means 0! = 1
factorialsWithTuple :: [Integer]
factorialsWithTuple = map snd . scanl (const . next) (0,1) $ factorialsWithTuple
  where next (x, y) = let x' = x + 1 in (x', x' * y)

-- similar solution with map only
factorialsWithTupleAndMap :: [Integer]
factorialsWithTupleAndMap = map snd fs
  where fs = (0,1):map next fs
        next (x, y) = let x' = x + 1 in (x', x' * y)

-- The following solution can be somewhat tricky to reason.
-- What if we add a restriction?
-- We are not allowed to keep track of the current factorial index, nor use tuples.
-- How do we get the next factorial result based on the two previous factorial results only?
-- In this case we need the first two values to start: 0! and 1!
factorials :: [Integer]
factorials = 1:scanl (\y z -> (y + z) * (y `div` z)) 1 factorials

-------------
-- Solution reasoning:
-------------
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
