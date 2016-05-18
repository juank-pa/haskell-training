module Exercises where

-- Parenthesization
-- 1. 2 + (2 * 3) - 1
-- 2. (^) 10 (1 + 1)
-- 3. ((2 ^ 2) * (4 ^ 5)) + 1

-- 1. 2.

-- 1. 1135, 1135, -1110, 1110
-- 2.
-- 3. 3375
-- 4. 5. 6.

waxOn = x * 5
  where
    x = y ^ 2
    y = z + 8
    z = 7

triple x = x * 3

waxOff x = triple x

