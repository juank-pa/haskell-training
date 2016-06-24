{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Chapter02.Exercises.Intermission where

----------------------
-- Comprehension Check
----------------------
-- 1. Change to use in REPL
half x = x / 2 -- changes into
-- let half x = x / 2

square x = x * x -- changes into
-- let square x = x * x

-- 2. Create function for
-- 3.14 * (5 * 5)
-- 3.14 * (10 * 10)
-- ...
circleArea r = 3.14 * (r * r)

------------------------------
-- Parentheses ans Association
------------------------------
-- 1. Parentheses changes result
--    8 + 7 * 9
--    (8 + 7) * 9
--
-- 2. Parentheses does not changes result
--    perimeter x y = (x * 2) + (y * 2)
--    perimeter x y = x * 2 + y * 2
--
-- 3. Parentheses changes result
--    f x = x / 2 + 9
--    f x = x / (2 + 9)

----------------
-- Heal the Sick
----------------
-- 1. let area x = 3. 14 * (x * x)
--    let area x = 3.14 * (x * x) -- removed space
--
-- 2. let double x = b * 2
--    let double x = x * 2 -- replaced b with x
--
-- 3. x = 7
--     y = 10
--    f = x + y
--    removed initial space in second line
x' = 7
y' = 10
f = x' + y'

--------------
-- A Head Code
--------------
-- 1. let x = 5 in x -- returns 5
-- 2. let x = 5 in x * x -- returns 25
-- 3. let x = 5; y = 6 in x * y -- returns 30
-- 4. let x = 3; y = 1000 in x + 3 -- returns 6

--- Now rewrite the previous code with where clauses
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

--------------------------------
-- Exercises with let and where!
--------------------------------
-- Rewrite with where clauses:
-- 1. let x = 3; y = 1000 in x * 3 + y
f1 = x * 3 + y
  where x = 3
        y = 1000

-- 2. let y = 10; x = 10 * 5 + y in x * 5
f2 = x * 5
  where y = 10
        x = 10 * 5 + y

-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y
f3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10
