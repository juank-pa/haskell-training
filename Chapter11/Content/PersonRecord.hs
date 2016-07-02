{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter11.Content.PersonRecord where

data Person
  = Person { name :: String
           , age :: Int
           } deriving (Eq, Show)

-- these are just sample data
jm = Person "julie" 108

nm = name jm
ag = age jm
