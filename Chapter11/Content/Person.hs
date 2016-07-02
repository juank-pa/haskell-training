{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter11.Content.Person where

data Person = MkPerson String Int deriving (Eq, Show)

-- these are just sample data
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s
