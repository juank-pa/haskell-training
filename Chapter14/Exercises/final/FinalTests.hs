module FinalTests where

import Test.QuickCheck
import Data.Char (toUpper)
import Data.List (sort)

--------------------------------
-- Validating numbers into words
--------------------------------
-- See Chapter14/Exercises/word-number

-------------------
-- Using QuickCheck
-------------------

-- Following QuickCheck properties use concrete types, either in the
-- property function or in the QuickCheck test itself, because otherwise
-- Arbitrary would not know which value to produce.

-- 1.
half x = x / 2

halfIdentity = (*2) . half

prop_halfIdentity x = halfIdentity x == (x :: Double)

testHalfIdentity :: IO ()
testHalfIdentity = quickCheck prop_halfIdentity

-- 2.
-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

testListOrderedInt :: IO ()
testListOrderedInt = quickCheck (listOrdered . sort :: [Int] -> Bool)

testListOrderedStr :: IO ()
testListOrderedStr = quickCheck (listOrdered . sort :: String -> Bool)

-- 3.
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y =
  x + y == y + x

prop_plusAssociative :: (Int, Int, Int) -> Bool
prop_plusAssociative (x, y, z) = plusAssociative x y z

prop_plusCommutative :: (Double, Double) -> Bool
prop_plusCommutative = uncurry plusCommutative

testPlusAssociative :: IO ()
testPlusAssociative = quickCheck prop_plusAssociative

testPlusCommutative :: IO ()
testPlusCommutative = quickCheck prop_plusCommutative

-- 4.
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y =
  x * y == y * x

prop_multAssociative :: (Int, Int, Int) -> Bool
prop_multAssociative (x, y, z) = multAssociative x y z

prop_multCommutative :: (Double, Double) -> Bool
prop_multCommutative = uncurry multCommutative

testMultAssociative :: IO ()
testMultAssociative = quickCheck prop_multAssociative

testMultCommutative :: IO ()
testMultCommutative = quickCheck prop_multCommutative

-- 5.
-- Researched the QuickCheck documentation to find the NonZero type
-- that guarantees arbitrary values excluding zero to prevent
-- division by zero exceptions.
prop_quotRemLaws :: (Int, NonZero Int) -> Bool
prop_quotRemLaws (x, NonZero y) = quot x y * y + rem x y == x

prop_divModLaws :: (Int, NonZero Int) -> Bool
prop_divModLaws (x, NonZero y) = div x y * y + mod x y == x

testQuotRemLaws :: IO ()
testQuotRemLaws = quickCheck prop_quotRemLaws

testDivModLaws :: IO ()
testDivModLaws = quickCheck prop_divModLaws

-- 6.
-- Power is NOT associative, nor commutative
powAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

powCommutative x y =
  x ^ y == y ^ x

prop_powAssociative :: (Int, Int, Int) -> Bool
prop_powAssociative (x, y, z) = powAssociative x y z

prop_powCommutative :: (Int, Int) -> Bool
prop_powCommutative = uncurry powCommutative

testPowAssociative :: IO ()
testPowAssociative = quickCheck prop_powAssociative

testPowCommutative :: IO ()
testPowCommutative = quickCheck prop_powCommutative

-- 7.
{-# ANN module "HLint: ignore Avoid reverse" #-}
prop_reverseIdentity :: Eq a => [a] -> Bool
prop_reverseIdentity x = (reverse . reverse) x == x

testReverseIdentity :: IO ()
testReverseIdentity = quickCheck (prop_reverseIdentity :: [Int] -> Bool)

-- or directly

testReverseIdentity2 :: IO ()
testReverseIdentity2 = quickCheck (\x -> (reverse . reverse) x == (x :: [Int]))

-- 8.
{-# ANN module "HLint: ignore Redundant $" #-}
prop_applicationIdentity :: Eq b => (a -> b) -> a -> Bool
prop_applicationIdentity f x = f x == (f $ x)

testApplicationIdentity :: IO ()
testApplicationIdentity = quickCheck (prop_applicationIdentity id :: Int -> Bool)

testApplicationIdentity' :: IO ()
testApplicationIdentity' = quickCheck (prop_applicationIdentity negate :: Int -> Bool)

prop_compositionIdentity :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
prop_compositionIdentity f g x = f (g x) == (f . g) x

testCompositionIdentity :: IO ()
testCompositionIdentity = quickCheck (prop_compositionIdentity negate ((1 :: Int)+))

-- 9.
-- This functions are not equal because the result of `foldr (:)`
-- reverses parameters while the result of `(++)` does not.
testAppendIdentity :: IO ()
testAppendIdentity = quickCheck
  (\(x, y) -> foldr (:) x (y :: String) == x ++ y)

-- This identity checks
-- foldr (++) [] == concat
{-# ANN module "HLint: ignore Use concat" #-}
testConcatIdentity :: IO ()
testConcatIdentity = quickCheck
  (\x -> foldr (++) [] (x :: [String]) == concat x)

-- 10.
f1 n xs = length (take n xs) == n

propF1 :: (Int, [a]) -> Bool
propF1 (n, xs) = f1 n xs

-- This test only checks when: `n <= length xs`
-- A property should check for all cases otherwise a restriction
-- must be introduced.
testF1 :: IO ()
testF1 = quickCheck (propF1 :: (Int, String) -> Bool)

-- 11.
f2 x = read (show x) == x

testF2 :: IO ()
testF2 = quickCheck (f2 :: Int -> Bool)

----------
-- Failure
----------

-- for a function
square x = x * x

-- why does this property not hold? Examine the type of sqrt.
squareIdentity = square . sqrt

-- The property fails because of floating point precision issues.
-- The type of squareIdentity inferes to `Fractional a => a -> a` and
-- defaults to `Double -> Double`
testSquareIdentity :: IO ()
testSquareIdentity = quickCheck (\x -> x == squareIdentity (x :: Double))

--------------
-- Idempotence
--------------
twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord s = map toUpper (take 1 s) ++ drop 1 s

-- 1.
-- Had to adapt the function because didn't work the way it was.
-- Both functions f3 and f4 are idempotent
f3 x =
  capitalizeWord x
  == twice capitalizeWord x
  && capitalizeWord x
  == fourTimes capitalizeWord x

testF3Idempotence :: IO ()
testF3Idempotence = quickCheck f3

-- 2.
f4 x =
  sort x
  == twice sort x
  && sort x
  == fourTimes sort x

testF4Idempotence :: IO ()
testF4Idempotence = quickCheck (f4 :: String -> Bool)

-----------------------------------------------
-- Make a Gen random generator for the datatype
-----------------------------------------------

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

foolFrequencyGen :: Gen Fool
foolFrequencyGen = frequency [(2, return Fulse), (1, return Frue)]

-- See Chapter14/Exercises/final/CipherTests.hs
