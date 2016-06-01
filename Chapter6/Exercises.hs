module Chapter6 where

import Data.List (sort)

-- Multiple Choice
-- 1. c)
-- 2. a) b)
-- 3. a)
-- 4. c)
-- 5. a)

-- Does it typecheck?
-- 1. Does not typecheck, lacks Show instance
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. Does not typecheck, lacks Eq instance
data Mood
  = Blah
  | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                 then Blah
                 else x

-- 3.
--  a) Blah and Woot
--  b) Type error because x got a concrete type of Mood when being compared to Woot
--  c) Type error because Mood is not implementing Ord

-- 4. Does typcheck
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?
-- Given:
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

-- 1. Doesn't typecheck: Papu expects instances of Rocks ad Yeah, not String and Bool
-- 2. Does typecheck
-- 3. Does typecheck
-- 4. Doesn't typecheck: Papu is not an instance of Ord

-- Match the types
-- 1. Doesn't compile because 1 is at least `Num`
i :: Num a => a
i = 1

-- i :: a
-- i = 1

-- 2. Doesn't compile because 1.0 is at least `Fractional`
f :: Float
f = 1.0

-- f :: Num a => a
-- f = 1.0

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

-- 7. Doesn't compile because `sigmund` returns an `Int` and its type expects a polymorphic value
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- sigmund :: a -> a
-- sigmund x = myX

-- 8. Doesn't compile because `sigmund'` returns an `Int` and its type expects a `Num`
sigmund' :: Int -> Int
sigmund' x = myX

-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

-- 9. Compiles because `Int` implements `Ord` which is expected by `sort`
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

jung2 :: [Int] -> Int
jung2 xs = head (sort xs)

-- 10. Does compile because all `sort` needs is an instance of `Ord` which `Char` is
young :: [Char] -> Char
young xs = head (sort xs)

young2 :: Ord a => [a] -> a
young2 xs = head (sort xs)

-- 11. Doesn't compile because `mySort` expects a `Char` and polymorphic `a` is too general
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- signifier :: Ord a => [a] -> a
-- signifier xs = head (mySort xs)

-- Type-Kwon-Do Two: Electric Typealoo
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = f y * fromInteger x
