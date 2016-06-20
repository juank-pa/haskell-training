module Chapter2.Exercises.Final where

-------------------
-- Parenthesization
-------------------
-- Add parentehses without changing the expression result
-- 1. 2 + 2 * 3 - 1
--    2 + (2 * 3) - 1
--
-- 2. (^) 10 $ 1 + 1
--    (^) 10 (1 + 1)
--
-- 3. 2 ^ 2 * 4 ^ 5 + 1
--    ((2 ^ 2) * (4 ^ 5)) + 1

-------------------------
-- Equivalent expressions
-------------------------
-- 1. 1 + 1 -- is equivalent to 2
-- 2. 10 ^ 2 -- is equivalent to 10 + 9 * 10
-- 3. 400 - 37 -- is NOT equivalent to (-) 37 400
-- 4. 100 `div` 3 -- is NOT equivalent to 100 / 3
-- 5. 2 * 5 + 18 -- is NOT equivalent to 2 * (5 + 18)

--------------------------
-- More fun with functions
--------------------------
-- Given:
-- z = 7
-- x = y ^ 2
-- waxOn = x * 5
-- y = z + 8
--
-- 1. What would the following return:
--    10 + waxOn -- returns 1135
--    (+10) waxOn -- returns 1135
--    (-) 15 waxOn -- returns -1110
--    (-) waxOn 15 -- returns 1110
-- 2.
-- 3. triple waxOn -- waxOn is a parameter for tripple, and returns 3375
-- 4. 5. 6. 7.

waxOn = x * 5
  where
    x = y ^ 2
    y = z + 8
    z = 7

triple x = x * 3

{-# ANN module "HLint: ignore Eta reduce" #-}
waxOff x = triple x
