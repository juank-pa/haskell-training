module Chapter07.Content.ReturnBroken where

returnBroke :: (((a -> b) -> c) -> d) -> d
returnBroke _ _ _ d = d
