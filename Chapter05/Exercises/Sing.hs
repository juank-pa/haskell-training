{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Chapter05.Exercises.Sing where

{-# ANN module "HLint: ignore Use String" #-}
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

-- Previous code was:
-- sing = if (x > y) then fstString x or sndString y
-- Changed `or` with `else`
{-# ANN module "HLint: ignore Redundant bracket" #-}
sing = if (x < y) then fstString x else sndString y
-- Here the where clause needed indentation
  where x = "Singin"
        y = "Somewhere"

-- 2. To make it sing the other song just change `>` with `<`
