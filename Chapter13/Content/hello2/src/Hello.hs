module Hello
  ( sayHello )
  where

{-# ANN module "HLint: ignore Redundant do" #-}
sayHello :: IO ()
sayHello = do
  putStrLn "hello world"
