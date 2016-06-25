module Chapter06.Content.Trivial1 where

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
