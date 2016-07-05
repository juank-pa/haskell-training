module Main where

import Hello

{-# ANN module "HLint: ignore Redundant do" #-}
main :: IO ()
main = do
  sayHello
