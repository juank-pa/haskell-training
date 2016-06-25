module Chapter06.Content.Identity2 where

data Identity a
  = Identity a

instance Ord a => Eq (Identity a) where
  (==) (Identity v) (Identity v') =
    compare v v' == EQ
