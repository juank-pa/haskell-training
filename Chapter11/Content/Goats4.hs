module Chapter11.Content.Goats4 where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

-- this will do the same thing as the
-- Int instance, but we still have to
-- define it separately
instance TooMany Goats where
  tooMany (Goats n) = tooMany n
