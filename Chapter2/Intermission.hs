module Intermission where

-- Comprehension Check
-- 1.
-- let half x = x / 2
-- let square x = x * x

-- 2.
circleArea r = 3.14 * (r * r)

-- Parentheses ans Association
-- 1. changes result
-- 2. does not changes result
-- 3. changes result

-- Heal the Sick
-- 1. let area x = 3.14 * (x * x) -- removed space
-- 2. let double x = x * 2 -- replaced b with x
-- 3. removed initial space in second line
x = 7
y = 10
f = x + y

-- A Head Code
-- 1. 5
-- 2. 25
-- 3. 30
-- 4. 6

five = x
  where x = 5

fiveSqr = x * x
  where x = 5

fiveBySix = x * y
  where
    x = 5
    y = 6

threePlusThree = x + 3
  where
    x = 3
    y = 1000

-- Exercises with let and where!
-- 1.
f1 = x * 3 + y
  where x = 3
        y = 1000

-- 2.
f2 = x * 5
  where y = 10
        x = 10 * 5 + y

-- 3.
f3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10
