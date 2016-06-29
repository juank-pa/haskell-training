module Chapter08.Content.Maybe where

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing
