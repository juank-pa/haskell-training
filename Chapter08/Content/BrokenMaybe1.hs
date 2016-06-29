module Chapter08.Content.BrokenMaybe1 where

f :: Bool -> Maybe Int
f False = 0
f _ = Nothing
