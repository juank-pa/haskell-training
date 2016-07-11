module Chapter08.Exercises.Final where

------------------
-- Review of types
------------------
-- 1. What is the type of [[True, False], [True, True], [False, True]]?
--    d) [[Bool]]
-- 2. Which one has the same type of [[True, False], [True, True], [False, True]]?
--    b) [[3 == 3], [6 > 5], [3 < 4]]
-- 3. Given:
func :: [a] -> [a] -> [a]
func x y = x ++ y
--    Which statemenet is true?
--    d) all of the above (x and y have the same type, they are both lists of Char (Strings))
-- 4. A valid aplication of `func`above is:
--    b) func "Hello" "World"

---------------------
-- Reviewing currying
---------------------
-- Given
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- What is the value of:
-- 1. appedCatty "woohoo!"
--    "woops mrow woohoo!"
-- 2. frappe "1"
--    "1 mrow haha"
-- 3. frappe (appedCatty "2")
--    "woops mrow 2 mrow haha"
-- 4. appedCatty (frappe "blue")
--    "woops mrow blue mrow haha"
-- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
--    "pink mrow haha mrow green mrow woops mrow blue"
-- 6. cattyConny (flippy "Pugs" "are") "awesome"
--    "are mrow Pugs mrow awesome"

------------
-- Recursion
------------
-- 1. Evaluate dividedBy 15 2
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

-- 2. Write a recursive sum function: 1 + 2 .. (n-1) + n
sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' x = x + sum' (x - 1)

-- 3. Write a recursive mult function
mult :: (Num a, Eq a) => a -> a -> a
mult _ 0  = 0
mult x y = x + mult x (y - 1)

-------------------
-- Fixing dividedBy
-------------------
data DividedResult
  = Result (Integer, Integer)
  | DividedByZero
  deriving Show

-- This acts more like quotRem (because of the subtraction algorithm)
-- ratter than divMod which uses modulus arithmetics
dividedBy :: Integer -> Integer -> DividedResult
dividedBy _   0 = DividedByZero
dividedBy num denom = go (abs num) (abs denom) 0
  where
    go n d count
      | n < d && num > 0 && denom > 0 = Result (count, n)
      | n < d && num < 0 && denom > 0 = Result (negate count, negate n)
      | n < d && num < 0 && denom < 0 = Result (count, negate n)
      | n < d && num > 0 && denom < 0 = Result (negate count, n)
      | otherwise = go (n - d) d (count + 1)

-----------------------
-- McCarthy 91 function
-----------------------
mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 $ mc91 $ n + 11

---------------------
-- Numbers into words
---------------------
-- See Chapter08/Exercises/WordNumber.hs
