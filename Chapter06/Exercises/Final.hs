{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter06.Exercises.Final where

import Data.List (sort)

------------------
-- Multiple Choice
------------------
-- 1. The Eq class c) makes equality tests possible
-- 2. The typeclass Ord
--    b) is a subclass of Eq
--    a) allows any two values to be compared
-- 3. What is the type of >? a) Ord a => a -> a -> Bool
-- 4. In x = divMod 16 12 c) the type of 𝑥 is a tuple
-- 5. The typeclass Integral includes a) Int and Integer numbers

---------------------
-- Does it typecheck?
---------------------
-- 1.
-- data Person = Person Bool
--
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)
-- Does not typecheck, data lacks Show instance

data Person = Person Bool deriving Show

{-# ANN module "HLint: ignore Use print" #-}
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
-- data Mood
--   = Blah
--   | Woot deriving Show
-- settleDown x = if x == Woot then Blah else x
-- Does not typecheck, lacks Eq instance
data Mood
  = Blah
  | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                 then Blah
                 else x

-- 3. If you were able to get settleDown to typecheck:
--  a) Acceptable inputs are Blah and Woot
--  b) settleDown 9 throws type error because x has a concrete type of Mood
--     when being compared to Woot
--  c) Blah > Woot throws type error because Mood is not implementing Ord

-- 4. Does typcheck
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

------------------------------------------------
-- Given a datatype declaration, what can we do?
------------------------------------------------
-- Given:
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

-- 1. phew = Papu "chases" True
--    Doesn't typecheck: Papu expects instances of Rocks ad Yeah, not String and Bool
--
-- 2. truth = Papu (Rocks "chomskydoz")
--                 (Yeah True)
--    Does typecheck

-- 3. equalityForall :: Papu -> Papu -> Bool
--    equalityForall p p' = p == p'
--    Does typecheck
--
-- 4. comparePapus :: Papu -> Papu -> Bool
--    comparePapus p p' = p > p'
--    Doesn't typecheck: Papu is not an instance of Ord

------------------
-- Match the types
------------------
-- 1. Try to change to:
--    i :: a
--    i = 1
--    Doesn't compile because 1 is at least `Num`
i :: Num a => a
i = 1

-- 2. Try to change to:
--    f :: Num a => a
--    f = 1.0
--    Doesn't compile because 1.0 is at least `Fractional`
f :: Float
f = 1.0


-- 3. Does compile because 1.0 is in fact `Fractional`
f' :: Float
f' = 1.0

f2' :: Fractional a => a
f2' = 1.0

-- 4. Does compile because `RealFrac` is a subclass of `Fractional`
--    and `Real` is a subclass of `Num`
f'' :: Float
f'' = 1.0

f2'' :: RealFrac a => a
f2'' = 1.0

-- 5. Does compile because `Ord` is a constrained subset of the parametric polymorphism
freud :: a -> a
freud x = x

freud2 :: Ord a => a -> a
freud2 x = x

-- 6. Does compile because `Int` is a concrete subset of parametric polymorphism
freud' :: a -> a
freud' x = x

freud2' :: Int -> Int
freud2' x = x

-- 7. Try to change to:
--    sigmund :: a -> a
--    sigmund x = myX
--    Doesn't compile because `sigmund` returns an `Int` and its type expects a polymorphic value

myX = 1 :: Int

sigmund :: Int -> Int
sigmund _ = myX

-- 8. Try yo change to:
--    sigmund' :: Num a => a -> a
--    sigmund' x = myX
--    Doesn't compile because `sigmund'` returns an `Int` and its type expects a `Num`
sigmund' :: Int -> Int
sigmund' _ = myX

{-# ANN module "HLint: ignore Use minimum" #-}
-- 9. Compiles because `Int` implements `Ord` which is expected by `sort`
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

jung2 :: [Int] -> Int
jung2 xs = head (sort xs)

{-# ANN module "HLint: ignore Use String" #-}
-- 10. Does compile because all `sort` needs is an instance of `Ord` which `Char` is
young :: [Char] -> Char
young xs = head (sort xs)

young2 :: Ord a => [a] -> a
young2 xs = head (sort xs)

-- 11. Try to change to:
--     signifier :: Ord a => [a] -> a
--     signifier xs = head (mySort xs)
--     Doesn't compile because `mySort` expects a `Char` and polymorphic `a` is too general
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

--------------------------------------
-- Type-Kwon-Do Two: Electric Typealoo
--------------------------------------
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn x y = fn x == y

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn x y = fn y * fromInteger x
