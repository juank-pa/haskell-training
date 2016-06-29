module Chapter08.Content.BrokenMaybe2 where

f :: Bool -> Maybe Int
f False = 0 :: Int
f _ = Nothing
