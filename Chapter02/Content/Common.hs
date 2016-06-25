{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chapter02.Content.Common where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

-- See Chapter02/Content/Learn.hs

foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z

-- See Chapter02/Content/FunctionWithwhere.hs
-- See Chapter02/Content/FunctionWithLet.hs
