module Intermission where

-- 1.
-- Translated the (+1) section to common math notation to the right to simplify parentheses
--
-- applyTimes 5 (+1) 5 =
--   (applyTimes (5 - 1) (+1) 5) + 1
--         (applyTimes 4 (+1) 5) + 1
--   (applyTimes (4 - 1) (+1) 5) + 1 + 1
--         (applyTimes 3 (+1) 5) + 1 + 1
--   (applyTimes (3 - 1) (+1) 5) + 1 + 1 + 1
--         (applyTimes 2 (+1) 5) + 1 + 1 + 1
--   (applyTimes (2 - 1) (+1) 5) + 1 + 1 + 1 + 1
--         (applyTimes 1 (+1) 5) + 1 + 1 + 1 + 1
--   (applyTimes (1 - 1) (+1) 5) + 1 + 1 + 1 + 1 + 1
--         (applyTimes 0 (+1) 5) + 1 + 1 + 1 + 1 + 1
--                             5 + 1 + 1 + 1 + 1 + 1
--                             10

fib :: Integral a => Int -> a
fib = (fibs!!)

fibs :: Integral a => [a]
fibs = fst <$> iterate ((,) <$> snd <*> uncurry (+)) (0,1)

fibs' :: Integral a => [a]
fibs' = 0:1:zipWith (+) fibs (tail fibs)

fibs'' :: [Integer]
fibs'' = fibs2' 0 1
  where fibs2' f1 f2 = f1 : fibs2' f2 (f1 + f2)
