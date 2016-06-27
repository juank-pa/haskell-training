module Chapter07.Content.GreetIfCool3 where

{-# ANN module "HLint: ignore Use if" #-}
greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eyyyyy. What's shakin'?"
    False -> putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"
