module Chapter7.Exercises.Final where

-- Multiple choice
-- 1. d)
-- 2. b)
-- 3. d)
-- 4. b)
-- 5. a)

-- Let's write code
-- 1.
-- a)
tensDigit :: Integral a => a -> (a, a)
tensDigit x = d
  where xLast = x `divMod` 10
        d     = fst xLast `divMod` 10

-- b) Depends on whether you want to keep function types or prefer to return a tuple
--    here I have preferred to change the type to accomodate more div and digit
-- c)
hunsDigit :: Integral a => a -> (a, a)
hunsDigit x = d2
  where
    d  = tensDigit x
    d2 = fst d `divMod` 10

-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True  -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b         = x
  | otherwise = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4. 5. 6. See Arith4.hs
