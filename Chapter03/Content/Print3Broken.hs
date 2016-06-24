-- print3broken.hs
module Chapter3.Content.Print3Broken where

{-# ANN module "HLint: ignore Redundant do" #-}
printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting printSecond
  where greeting = "Yarrrrr"
