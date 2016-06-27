{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chapter07.Exercises.Intermission where

-----------
-- Crab Bag
-----------
-- 1. All are equivalent
mTh :: Num a => a -> a -> a -> a
mTh x y z = x * y * z

{-# ANN module "HLint: ignore Redundant lambda" #-}
mTh' :: Num a => a -> a -> a -> a
mTh' x y = \z -> x * y * z

{-# ANN module "HLint: ignore Collapse lambdas" #-}
mTh'' :: Num a => a -> a -> a -> a
mTh'' x = \y -> \z -> x * y * z

mTh''' :: Num a => a -> a -> a -> a
mTh''' = \x -> \y -> \z -> x * y * z

-- 2. The type of `mTh 3` is d) Num a => a -> a -> a
-- 3. Rewrite as anonymous lambdas
-- a)
{-# ANN module "HLint: ignore Use if" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  -- before: where f n = n + 1
  where f = \n -> n + 1
-- b) addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5
-- c) Rewrite so it doesn't use anonymous lambda syntax:
--    mflip f = \x -> \y -> f y x
mflip f x y = f y x

---------------
-- Variety Pack
---------------
-- 1.
k (x, y) = x
k1 = k (4 - 1, 10)
k2 = k ("three", 1 + 2)
k3 = k (3, True)

-- a) The type of k is: (a, b) -> a
-- b) The type of k2 is [Char], and is different from k1 and k3 which type is Num a => a.
--    For some reason GHCi says Integer for the type of k1 and k3
-- c) k3 returns 3 as result
-- 2. Definition for:
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

----------------
-- Case Practice
----------------
-- Rewrite with case
-- 1. functionC x y = if (x > y) then x else y
{-# ANN module "HLint: ignore Redundant bracket" #-}
functionC x y =
  case (x > y) of
    True ->  x
    False -> y
-- 2. ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n =
  case even n of
    True  -> (n+2)
    False -> n


-- 1. Fix:
--    nums x =
--      case compare x 0 of
--        LT -> -1
--        GT -> 1
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

---------------
-- Artful Dodgy
---------------
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- 1. dodgy 1 0 -- returns 1
-- 2. dodgy 1 1 -- returns 11
-- 3. dodgy 2 2 -- returns 22
-- 4. dodgy 1 2 -- returns 21
-- 5. dodgy 2 1 -- returns 12
-- 6. oneIsOne 1 -- returns 11
-- 7. oneIsOne 2 -- returns 21
-- 8. oneIsTwo 1 -- returns 21
-- 9. oneIsTwo 2 -- returns 22
-- 10. oneIsOne 3 -- returns 31
-- 11. oneIsTwo 3 -- returns 23

-------------
-- Guard Duty
-------------
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100
-- 1. If you replace the first guard with | otherwise = 'F', all inputs will return 'F'
-- 2. Moving | y >= 0.7 = 'C' to the top will yield 'C' for all scores 70 and up.
--    Cases for 'A and 'B' will never reach
-- 3.
pal xs
  | xs == reverse xs = True
  | otherwise = False
-- returns b) True when x is a palindrome
-- 4. `pal` can receive a list with elements of any instance of Eq
-- 5. `pal` type is: Eq a => [a] -> Bool
-- 6.
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
-- returns c) an indication of whether its number argument is positive, negative, or zero
-- 7. `number` can take any instance of Num and Ord
