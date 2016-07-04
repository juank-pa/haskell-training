module Chapter12.Content.EvenAdd1 where

-- ifEvenAdd2 :: Integer -> Integer
-- ifEvenAdd2 n = if even n then n+2 else ???

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then n+2 else Nothing
