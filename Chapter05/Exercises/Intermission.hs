{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Chapter05.Exercises.Intermission where

----------------
-- Type Matching
----------------
-- 1. 2.
--  a) not    c) _ :: Bool -> Bool
--  b) length d) _ :: [a] -> Int
--  c) concat b) _ :: [[a]] -> [a]
--  d) head   a) _ :: [a] -> a
--  e) (<)    e) _ :: Ord a => a -> a -> Bool

-----------------
-- Type Arguments
-----------------
-- 1. If:
--    f :: a -> a -> a -> a
--    x :: Char
--    f x
--    a) Char -> Char -> Char
--
-- 2. If:
--    g :: a -> b -> c -> b
--    g 0 'c' "woot"
--    d) Char
--
-- 3. If:
--    h :: (Num a, Num b) => a -> b -> b
--    h 1.0 2
--    d) Num b => b
--
-- 4. If:
--    h :: (Num a, Num b) => a -> b -> b
--    h 1 (5.5 :: Double)
--    c) Double
--
-- 5. If:
--    jackal :: (Ord a, Eq b) => a -> b -> a
--    jackal "keyboard" "has the word jackal in it"
--    a) [Char]
--
-- 6. If:
--    jackal :: (Ord a, Eq b) => a -> b -> a
--    jackal "keyboard"
--    e) Eq b => b -> [Char]
--
-- 7. If:
--    kessel :: (Ord a, Num b) => a -> b -> a
--    kessel 1 2
--    d) (Num a, Ord a) => a
--
-- 8. kessel :: (Ord a, Num b) => a -> b -> a
--    kessel 1 (2 :: Integer)
--    a) (Num a, Ord a) => a
--
-- 9. kessel :: (Ord a, Num b) => a -> b -> a
--    kessel (1 :: Integer) 2
--    c) Integer

----------------
-- Parametricity
----------------
-- 1. Impossible to create a function that does something different than
--    returning the same value for: id :: a -> a
-- 2. Can't violate constraints for: f1 :: a -> a -> a
--    Two possible implementations
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

-- 3. Only one implementation is possible for: a -> b -> b
--    Behaviour doesn't change if sent parameter types for a and b change
f3 :: a -> b -> b
f3 x y = y

-----------------
-- Apply Yourself
-----------------
-- Write the function type:
-- 1. If:
--    (++) :: [a] -> [a] -> [a]
--    myConcat x = x ++ " yo"
--    myConcat :: [Char] -> [Char]
--
-- 2. If:
--    (*) :: Num a => a -> a -> a
--    myMult x = (x / 3) * 5
--    myMult :: Fractional a => a -> a
--
-- 3. If:
--    take :: Int -> [a] -> [a]
--    myTake x = take x "hey you"
--    myTake :: Int -> [Char]
--
-- 4. If:
--    (>) :: Ord a => a -> a -> Bool
--    myCom x = x > (length [1..10])
--    myCom :: Int -> Bool
--
-- 5. If:
--    (<) :: Ord a => a -> a -> Bool
--    myAlpha x = x < 'z'
--    myAlpha :: Char -> Bool
