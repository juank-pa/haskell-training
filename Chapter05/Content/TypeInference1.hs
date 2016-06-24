-- typeInference1.hs
module Chapter05.Exercises.TypeInference1 where

f :: Num a => a -> a -> a
f x y = x + y + 3
