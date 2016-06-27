{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Chapter07.Content.BindExp3 where

bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in
              "the integer was: " ++ show x
              ++ " and y was: " ++ show y


