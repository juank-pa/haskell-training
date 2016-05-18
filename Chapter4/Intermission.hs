module Intermission where

-- 1. Mood
-- 2. Blah and Woot
-- 3. Signature should be: changeMood :: Mood -> Mood
--    Woot is a data constructor not a type constructor
-- 4. changeMood Woot = Blah
--    changeMood _    = Woot

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood _    = Woot

-- 1. not True && True -- true is not a valid value
-- 2. not (x == 6) -- double equals instead of just one
-- 3. Right
-- 4. "Merry" > "Happy" -- double quotes instead of brackets
-- 5. ['1','2','3'] ++ "look at me!" -- cannot append [Num] to [Char]
--    or "123" ++ "look at me!"


