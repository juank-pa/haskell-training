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
