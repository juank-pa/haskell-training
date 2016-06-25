module Chapter06.Content.Trivial2 where

data Trivial =
  Trivial'

instance Eq Trivial where
  (==) Trivial' Trivial' = True
