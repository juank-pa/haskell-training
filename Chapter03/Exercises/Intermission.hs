{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter3.Exercises.Intermission where

--------
-- Scope
--------
-- 1. Yes, y is in scope for z
-- Prelude> let x = 5
-- Prelude> let y = 7
-- Prelude> let z = x * y
--
-- 2. No, h is not in scope for g
-- Prelude> let f = 3
-- Prelude> let g = 6 * f + h
--
-- 3. No, because d is not in scope for r.
--    d is local to area.
-- area d = pi * (r * r)
-- r = d / 2
--
-- 4. Yes, r is in scope for d
area d = pi * (r * r)
  where r = d / 2

----------------
-- Syntax Errors
----------------
-- 1. ++ [1, 2, 3] [4, 5, 6]
--    (++) [1, 2, 3] [4, 5, 6] -- fix
-- 2. '<3' ++ ' Haskell'
--    "<3" ++ " Haskell" -- fix
-- 3. concat ["<3", " Haskell"] -- No need to fix
