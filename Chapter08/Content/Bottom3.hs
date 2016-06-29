module Chapter08.Content.Bottom3 where

f :: Bool -> Int
f False = 0
f _ = error $ "*** Exception: "
              ++ "Non-exhaustive"
              ++ "patterns in function f"
