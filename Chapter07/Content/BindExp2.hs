{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chapter07.Content.BindExp2 where

bindExp :: Integer -> String
bindExp x = let z = y + x in
            let y = 5 in "the integer was: "
            ++ show x ++ " and y was: "
            ++ show y ++ " and z was: " ++ show z
