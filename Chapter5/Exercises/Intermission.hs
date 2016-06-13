module Intermission where

-- Type Matching
-- 1. 2.
--  a) -> c)
--  b) -> d)
--  c) -> b)
--  d) -> a)
--  e) -> e)

-- Type Arguments
-- 1. a)
-- 2. d)
-- 3. d)
-- 4. c)
-- 5. a)
-- 6. e)
-- 7. d)
-- 8. a)
-- 9. c)

-- Parametricity
-- 1. Impossible
-- 2. Can't violate constraints
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

-- 3. Only one implementation and behaviour doesn't change
f3 :: a -> b -> b
f3 x y = y

-- Apply Yourself
-- 1. myConcat :: [Char] -> [Char]
-- 2. myMult :: Fractional a => a -> a
-- 3. myTake :: Int -> [Char]
-- 4. myCom :: Int -> Bool
-- 5. myAlpha :: Char -> Bool
