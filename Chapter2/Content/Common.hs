module Chapter2.Content.Common where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z
