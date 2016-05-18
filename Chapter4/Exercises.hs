module Exercises where

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1. [a] -> Int
-- 2a. 5
-- 2b. 3
-- 2c. 2
-- 2d. 5
-- 3. The second expression won't work because length returns and Integer and the (/) operator
--    expects two Fractional numbers.
-- 4. 6 `div` length [1, 2, 3]
-- 5. Type is Bool and result is True
-- 6. Type is Bool and result is False
-- 7.
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

-- Correcting Syntax
-- 1. Use backticks for x instead of quotes and lowercase function identifier

x = (+)

f' xs = w `x` 1
  where w = length xs

-- 2. \x -> x -- Arrow instead of equal sign and lowercase x
-- 3. \(x:xs) -> x -- Missing parenthesis
-- 4.

f'' (a, b) = a

-- Match function names to their types
-- 1. c)
-- 2. b)
-- 3. a)
-- 4. d)
