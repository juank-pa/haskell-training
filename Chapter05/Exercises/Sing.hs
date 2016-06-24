{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Chapter05.Exercises.Sing where

{-# ANN module "HLint: ignore Use String" #-}
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

{-# ANN module "HLint: ignore Redundant bracket" #-}
sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
