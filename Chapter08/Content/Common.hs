module Chapter08.Content.Common where

{-# ANN module "HLint: ignore Evaluate" #-}
fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

-- See Chapter08/Content/BrokenFact.hs
-- See Chapter08/Content/Factorial.hs

inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
-- different syntax, same thing
three' = (inc . inc . inc) 0

{-# ANN module "HLint: ignore Redundant bracket" #-}
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

{-# ANN module "HLint: ignore Eta reduce" #-}
incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

applyTimes' :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes (n-1) f $ b

-- See Chapter08/Content/Bottom1.hs
-- See Chapter08/Content/Bottom2.hs
-- See Chapter08/Content/Bottom3.hs

-- See Chapter08/Content/Maybe.hs
-- See Chapter08/Content/BrokenMaybe1.hs
-- See Chapter08/Content/BrokenMaybe2.hs

-- See Chapter08/Content/Fibonacci1.hs
-- See Chapter08/Content/Fibonacci2.hs
-- See Chapter08/Content/Fibonacci3.hs
-- See Chapter08/Content/Fibonacci4.hs

-- See Chapter08/Content/Divide1.hs
-- See Chapter08/Content/Divide2.hs
-- See Chapter08/Content/Divide3.hs

-- not recursive
lessOne :: Int -> Int
lessOne n = n - 1
-- recursive
zero :: Int -> Int
zero 0 = 0
zero n = zero (n - 1)
