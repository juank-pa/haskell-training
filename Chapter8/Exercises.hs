module Exercises where

-- Review of types
-- 1. d)
-- 2. b)
-- 3. d)
func :: [a] -> [a] -> [a]
func x y = x ++ y
-- 4. b)

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. "are mrow Pugs mrow awesome"

-- Recursion
-- 1.
-- dividedBy 15 2
--   go 15 2 0
--   go (15 - 2) 2 (0 + 1)
--   go 13 2 1
--   go (13 - 2) 2 (1 + 1)
--   go 11 2 2
--   go (11 - 2) 2 (2 + 1)
--   go 9 2 3
--   go (9 - 2) 2 (3 + 1)
--   go 7 2 4
--   go (7 - 2) 2 (4 + 1)
--   go 5 2 5
--   go (5 - 2) 2 (5 + 1)
--   go 3 2 6
--   go (3 - 2) 2 (6 + 1)
--   go 1 2 7
--   (7, 1)

-- 2.
sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' x = x + sum' (x - 1)

-- 3.
mult :: Integral a => a -> a -> a
mult x 0  = 0
mult x x' = x + mult x (x' - 1)

-- Fixing dividedBy

data DividedResult
  = Result (Integer, Integer)
  | DividedByZero
  deriving Show

-- This acts more like quotRem (because of the subtraction algorithm)
-- ratter than divMod which uses modulus arithmetics
dividedBy :: Integer -> Integer -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom = go (abs num) (abs denom) 0
  where
    go n d count
      | n < d && num > 0 && denom > 0 = Result (count, n)
      | n < d && num < 0 && denom > 0 = Result (negate count, negate n)
      | n < d && num < 0 && denom < 0 = Result (count, negate n)
      | n < d && num > 0 && denom < 0 = Result (negate count, n)
      | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function
mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 $ mc91 $ n + 11

-- Numbers into words
-- Solved in WordNumber.hs
