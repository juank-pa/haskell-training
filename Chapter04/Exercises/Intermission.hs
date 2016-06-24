module Chapter4.Exercises.Intermission where

-------------
-- Mood Swing
-------------
-- 1. Mood is the type constructor
-- 2. Blah and Woot are the data constructors, values or constants
-- 3. What's wrong with: changeMood :: Mood -> Woot?
--    Signature should be: changeMood :: Mood -> Mood
--    Woot is a data constructor not a type constructor
-- 4. Fix:
--    changeMood Woot = Blah
--    changeMood _    = Woot

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood _    = Woot

--------------------
-- Find the Mistakes
--------------------
-- 1. not True && true
--    not True && True -- true is not a valid value
-- 2. not (x = 6)
--    not (x == 6) -- double equals instead of just one
-- 3. (1 * 2) > 5 -- is right
-- 4. [Merry] > [Happy]
--    "Merrr" > "Happy" -- double quotes instead of brackets
-- 5. [1, 2, 3] ++ "look at me!"
--    ['1','2','3'] ++ "look at me!" -- cannot append [Num] to [Char]
--    "123" ++ "look at me!" -- other solution
