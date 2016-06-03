module Intermission where

import Data.Bool

-- EnumFromTo

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

eftChar :: Char -> Char -> [Char]
eftChar s e
  | s == e    = [e]
  | otherwise = s : eftChar (succ s) e

-- Thy Fearful Symmetry
-- 1.
myWords :: String -> [String]
myWords "" = []
myWords xs = ln : rest
  where
    ch   = ' '
    ln   = takeWhile (/=ch) xs
    rest = (myWords . dropWhile (==ch) . dropWhile (/=ch)) xs

-- 2. See PoemLines.hs
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


-- Comprehend Thy Lists
-- [4, 16]
-- [] -- the y > 50 predicate excludes all elements
-- [] -- the same here even after using take 5

-- Square Cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
-- 1.
r1 = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
r2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3.
r3 = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]


-- Bottom Madness
-- Will it blow up?
-- 1.  Bottom
-- 2.  Returns [1]
-- 3.  Bottom
-- 4.  Returns 3
-- 5.  Bottom
-- 6.  Returns [2]
-- 7.  Bottom
-- 8.  Returns [1]
-- 9.  Returns [1,3]
-- 10. Bottom

-- Is it in normal form?
-- 1. NF
-- 2. WHNF
-- 3. Neither
-- 4. Neither
-- 5. Neither
-- 6. Neither (even if ['a'..'z'] is WHNF the outter ++ is not)
-- 7. WHNF

-- More Bottoms
-- 1. Bottom
-- 2. Returns [2]
-- 3. Bottom
-- 4. Given a String it returns a list of Bools with True for vocals and False otherwise
-- 5.
-- a) [1,4,9,16,25,36,49,64,81,100] -- Square of numbers from 1 to 10
-- b) [1,10,20] -- a list with the minimum numbers for each range
-- c) [15,15,15]
-- 6.
negateWhen :: Num a => (a -> Bool) -> [a] -> [a]
negateWhen f = map (\x -> bool x (-x) (f x))

r4 = negateWhen (==3) [1..10]

-- or directly
r5 = map (\x -> bool x (-x) (x==3)) [1..10]

-- Filtering
-- 1.
multiplesOf :: Integral a => a -> [a] -> [a]
multiplesOf n = filter ((==0).(`rem`n))

r6 = multiplesOf 3 [1..30]

-- or directly
r7 = filter ((==0).(`rem` 3)) [1..30]

-- 2.
r8 = (length . multiplesOf 3) [1..30]
-- or if using the direct expression
r9 = (length . filter ((==0).(`rem` 3))) [1..30]

-- 3.
myFilter :: String -> [String]
myFilter = filter (`notElem`["the","an","a"]) . words

-- Zipping exercises
-- 1.
myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
myZip _ _           = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _           = []

myZip' :: [a] -> [b] -> [(a,b)]
myZip' = myZipWith (,)
