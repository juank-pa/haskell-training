{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter02.Content.FunctionWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2
