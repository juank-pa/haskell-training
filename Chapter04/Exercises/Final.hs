{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter04.Exercises.Final where

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1. Type signature for length: [a] -> Int
-- 2. Lengths for:
-- a) length [1, 2, 3, 4, 5] -- 5
-- b) length [(1, 2), (2, 3), (3, 4)] -- 3
-- c) length allAwesome -- 2
-- d) length (concat allAwesome) -- 5
--
-- 3. 6 / 3
--    6 / length [1,2,3]
--    The second expression won't work because length returns and Int and
--    the (/) operator expects two Fractional numbers.
-- 4. 6 `div` length [1, 2, 3] -- to fix 3.
-- 5. 2 + 3 == 5 -- type is Bool and result is True
-- 6. let x = 5
--    x + 3 == 5
--    Type is Bool and result is False
-- 7. Which one works. If it works then reduce.
--
--   length allAwesome == 2
--   length [awesome,alsoAwesome] == 2
--   2 == 2
--   True
--
--   length [1, 'a', 3, 'b'] -- Error: all elements in the list must be of the same datatype
--
--   length allAwesome + length awesome
--   length [awesome, alsoAwesome] + length ["Papuchon", "curry", ":)"]
--   2 + 3
--   5
--
--   (8 == 8) && ('b' < 'a')
--   True && False
--   False
--
--   (8 == 8) && 9 -- Error: 9 is not a Bool
--
-- 8.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

-- 10.

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

--------------------
-- Correcting Syntax
--------------------
-- 1. x = (+)
--    F xs = w 'x' 1
--         where w = length xs
--    Fix: Use backticks for x instead of quotes and lowercase function identifier

x' = (+)

f' xs = w `x'` 1
  where w = length xs

-- 2. \X = x
--    \x -> x -- Arrow instead of equal sign and lowercase x
-- 3. \ x : xs -> x
--    \(x:xs) -> x -- Missing parenthesis
-- 4. f (a b) = A

f'' (a, _) = a

--------------------------------------
-- Match function names to their types
--------------------------------------
-- 1. The type of show is c) Show a => a -> String
-- 2. The type of (==) is b) Eq a => a -> a -> Bool
-- 3. The type of fst is a) (a, b) -> a
-- 4. The type of (+) is d) Num a => a -> a -> a
