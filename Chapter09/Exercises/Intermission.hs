{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Chapter09.Exercises.Intermission where

import Data.Bool

-------------
-- EnumFromTo
-------------

eftBool :: Bool -> Bool -> [Bool]
eftBool s e
  | s == e    = [e]
  | otherwise = s : eftBool (succ s) e

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd s e
  | s == e    = [e]
  | otherwise = s : eftOrd (succ s) e

eftInt :: Int -> Int -> [Int]
eftInt s e
  | s == e    = [e]
  | otherwise = s : eftInt (succ s) e

{-# ANN module "HLint: ignore Use String" #-}
eftChar :: Char -> Char -> [Char]
eftChar s e
  | s == e    = [e]
  | otherwise = s : eftChar (succ s) e

-- after abstracting similarities
eft :: (Enum a, Eq a) => a -> a -> [a]
eft s e
  | s == e    = [e]
  | otherwise = s : eft (succ s) e

eftBool' :: Bool -> Bool -> [Bool]
eftBool' = eft

eftOrd' :: Ordering -> Ordering -> [Ordering]
eftOrd' = eft

eftInt' :: Int -> Int -> [Int]
eftInt' = eft

eftChar' :: Char -> Char -> [Char]
eftChar' = eft

-----------------------
-- Thy Fearful Symmetry
-----------------------
-- 1. Separate strings into words using `takeWhile` and `dropWhile`
myWords :: String -> [String]
myWords "" = []
myWords xs = w : rest
  where
    ch   = ' '
    w    = takeWhile (/=ch) xs
    rest = (myWords . dropWhile (==ch) . dropWhile (/=ch)) xs

-- 2. See Chaper09/Exercises/PoemLines.hs
-- 3.
splitBy :: Char -> String -> [String]
splitBy ch "" = []
splitBy ch xs = token : rest
  where
    token = takeWhile (/=ch) xs
    rest  = (splitBy ch . dropWhile (==ch) . dropWhile (/=ch)) xs

myWords' :: String -> [String]
myWords' = splitBy ' '

myLines :: String -> [String]
myLines = splitBy '\n'


-----------------------
-- Comprehend Thy Lists
-----------------------
-- Given:
mySqr = [x^2 | x <- [1..5]]

-- [x | x <- mySqr, rem x 2 == 0] equals [4, 16]

-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- equals [] -- the y > 50 predicate excludes all elements

-- take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]
-- equals [] -- the same here even after using take 5

--------------
-- Square Cube
--------------
myCube = [y^3 | y <- [1..5]]
-- 1. combine `mySqr` and `myCube`
r1 = [(x, y) | x <- mySqr, y <- myCube]

-- 2. filter elements greater than 50
r2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3. get how many elements inhabit that list
r3 = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]


-----------------
-- Bottom Madness
-----------------
-- Will it blow up?
-- 1.  [x^y | x <- [1..5], y <- [2, undefined]]
--     Is bottom
-- 2.  take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
--     Returns [1]
-- 3.  sum [1, undefined, 3]
--     Is bottom
-- 4.  length [1, 2, undefined]
--     Returns 3
-- 5.  length $ [1, 2, 3] ++ undefined
--     Is bottom
-- 6.  take 1 $ filter even [1, 2, 3, undefined]
--     Returns [2]
-- 7.  take 1 $ filter even [1, 3, undefined]
--     Is bottom
-- 8.  take 1 $ filter odd [1, 3, undefined]
--     Returns [1]
-- 9.  take 2 $ filter odd [1, 3, undefined]
--     Returns [1,3]
-- 10. take 3 $ filter odd [1, 3, undefined]
--     Is bottom

------------------------
-- Is it in normal form?
------------------------
-- 1. [1, 2, 3, 4, 5] is NF
-- 2. 1 : 2 : 3 : 4 : _ is WHNF
-- 3. enumFromTo 1 10 is neither
-- 4. length [1, 2, 3, 4, 5] is neither
-- 5. sum (enumFromTo 1 10) is neither
-- 6. ['a'..'m'] ++ ['n'..'z'] is neither (even if ['a'..'z'] is WHNF the outter ++ is not)
-- 7. (_, 'b') is WHNF

---------------
-- More Bottoms
---------------
-- 1. take 1 $ map (+1) [undefined, 2, 3]
--    Is bottom
-- 2. take 1 $ map (+1) [1, undefined, 3]
--    Returns [2]
-- 3. take 2 $ map (+1) [1, undefined, 3]
--    Is bottom
-- 4. What does this do?
{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Use infix" #-}
itIsMystery xs = map (\x -> elem x "aeiou") xs
--    Given a String it returns a list of Bools with True for vocals and False otherwise
-- 5.
-- a) map (^2) [1..10]
--    Result: [1,4,9,16,25,36,49,64,81,100] -- Square of numbers from 1 to 10
-- b) map minimum [[1..10], [10..20], [20..30]]
--    Result: [1,10,20] -- a list with the minimum numbers for each range
-- c) map sum [[1..5], [1..5], [1..5]]
--    Result: [15,15,15]
-- 6.
negateWhen :: Num a => (a -> Bool) -> [a] -> [a]
negateWhen f = map (\x -> bool x (-x) (f x))

r4 = negateWhen (==3) [1..10]

-- or directly
r5 = map (\x -> bool x (-x) (x==3)) [1..10]

------------
-- Filtering
------------
-- 1. Multiples of 3
multiplesOf :: Integral a => a -> [a] -> [a]
multiplesOf n = filter ((==0).(`rem`n))

r6 = multiplesOf 3 [1..30]

-- or directly
r7 = filter ((==0).(`rem` 3)) [1..30]

-- 2. How many multiples of 3
r8 = (length . multiplesOf 3) [1..30]
-- or if using the direct expression
r9 = (length . filter ((==0).(`rem` 3))) [1..30]

-- 3. Filter articles
myFilter :: String -> [String]
myFilter = filter (`notElem`["the","an","a"]) . words

--------------------
-- Zipping exercises
--------------------
-- 1.
myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
myZip _ _           = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _           = []

myZip' :: [a] -> [b] -> [(a,b)]
myZip' = myZipWith (,)
