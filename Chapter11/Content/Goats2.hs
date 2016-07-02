module Chapter11.Content.Goats2 where

newtype Goats
  = Goats Int deriving (Eq, Show)
newtype Cows
  = Cows Int deriving (Eq, Show)

-- Now we can rewrite our type to be
-- safer, pattern matching in order
-- to access the Int inside our data
-- constructor Goats.
tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
