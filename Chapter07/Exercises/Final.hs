module Chapter7.Exercises.Final where

------------------
-- Multiple choice
------------------
-- 1. A polymorphic function d) may resolve to values of different types, depending on inputs
-- 2. `f` and `g` have types `Char -> String` and `String -> [String]` respectively
--    `g . f` has the type b) `Char -> [String]`
-- 3. f has the type `Ord a => a -> a -> Bool` and weapply it to one numeric value
--    The type is now d) `(Ord a, Num a) => a -> Bool`
-- 4. A function with the type (a -> b) -> c  b) is a higher-order function
-- 5. Give
--    f :: a -> a
--    f x = x
--    The type of f True is a) f True :: Bool

-------------------
-- Let's write code
-------------------
-- 1.
-- a) Rewrite using divMod
--
--    tensDigit :: Integral a => a -> a
--    tensDigit x = d
--      where xLast = x `div` 10
--            d     = xLast `mod` 10
--
-- IMHO, using `divMod` complicates things but anyway...
--
-- Could have been done like this:
-- tensDigit :: Integral a => a -> a
-- tensDigit x = d
--  where xLast = x `divMod` 10
--        d     = snd (fst xLast `divMod` 10)
--
-- But, to practice function composition and point-free style.
--
-- Note: Instead of implementing `tensDigit` with a fixed divisor of 10,
-- I used a helper function which can get any desired digit.
-- This way we can reuse it for `hunsDigit` as well.
getDigit :: Integral a => Int -> a -> a
getDigit p = snd . (`divMod` 10) . fst . (`divMod` (10 ^ p))

tensDigit :: Integral a => a -> a
tensDigit = getDigit 1
-- or
-- with pattern matching
tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast,_) = x `divMod` 10
        (_,d)     = xLast `divMod` 10

-- b) Does the divMod version have the same type as the original version?
--    Depends on whether you want to keep function types or return the
--    tens digit and the rest of the digits in a tuple instead.
-- c) Let's get the hundreds digit.
--    Keeping the `divMod` trend.
hunsDigit :: Integral a => a -> a
hunsDigit = getDigit 2
-- or
-- with pattern matching
hunsDigit' :: Integral a => a -> a
hunsDigit' x = d1
  where
    (d,_)  = x `divMod` 100
    (_,d1) = d `divMod` 10
-- there are other possible solutions as well

-- 2. Implement the function of type: a -> a -> Bool -> a
--    using case and guards
{-# ANN module "HLint: ignore Use if" #-}
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True  -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b         = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ True = x
foldBool3 _ y False = y

-- 3. Fill in the definition
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4. 5. 6. See Chapter07/Exercises/Arith4.hs
