module Chapter08.Exercises.Intermission where

-- 1. Evaluate: applyTimes 5 (+1) 5
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
