module Intermission where

-- Crab Bag
-- 1. All are equivalent
mTh :: Num a => a -> a -> a -> a
mTh x y z = x * y * z

mTh' :: Num a => a -> a -> a -> a
mTh' x y = \z -> x * y * z

mTh'' :: Num a => a -> a -> a -> a
mTh'' x = \y -> \z -> x * y * z

mTh''' :: Num a => a -> a -> a -> a
mTh''' = \x -> \y -> \z -> x * y * z

-- 2. d)
-- 3.
-- a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1
-- b)
addFive = \x -> \y -> (if x > y then y else x) + 5
-- c)
mflip f x y = f y x

-- Variety Pack
-- 1.
k (x, y) = x
k1 = k (4-1, 10)
k2 = k ("three", 1 + 2)
k3 = k (3, True)

-- a) (a, b) -> a
-- b) [Char], type is different from k1 and k3 which type is Num a => a.
--    For some reason GHCi says Integer
-- c) k3
-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Case Practice
-- 1.
functionC x y =
  case (x > y) of
    True ->  x
    False -> y
-- 2.
ifEvenAdd2 n =
  case even n of
    True  -> (n+2)
    False -> n


-- 1.
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Artful Dodgy
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- 1. 1
-- 2. 11
-- 3. 22
-- 4. 21
-- 5. 12
-- 6. 11
-- 7. 21
-- 8. 21
-- 9. 22
-- 10. 31
-- 11. 23

-- Guard Duty
-- 1. All inputs return 'F'
-- 2. Moving | y >= 0.7 = 'C' to the top will yield 'C' for all scores 70 and up
--    cases for 'A and 'B' will never reach
-- 3. b)
-- 4. A list with elements of any instance of Eq
-- 5. Eq a => [a] -> Bool
-- 6. c)
-- 7. Any instance of Eq and Ord
