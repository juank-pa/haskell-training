{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chapter07.Content.BindExp1 where

bindExp :: Integer -> String
bindExp x = let y = 5 in
              "the integer was: " ++ show x
              ++ " and y was: " ++ show y
