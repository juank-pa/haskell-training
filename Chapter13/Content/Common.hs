module Chapter13.Content.Common where

twoo :: IO Bool
twoo = do c  <- getChar
          c' <- getChar
          return (c == c')

{-# ANN module "HLint: ignore Use when" #-}
main :: IO ()
main = do c  <- getChar
          c' <- getChar
          if c == c'
            then putStrLn "True"
            else return ()
