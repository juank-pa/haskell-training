{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter02.Content.FunctionWithLet where

printInc2 n = let plusTwo = n + 2
              in print plusTwo

{-# ANN module "HLint: ignore Avoid lambda" #-}
printInc2' n =
  (\plusTwo -> print plusTwo) (n + 2)
