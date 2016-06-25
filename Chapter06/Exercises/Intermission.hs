module Chapter06.Exercises.Intermission where

-------------------
-- Tuple experiment
-------------------
-- quotRem returns a tuple with the quotient and remainder of its parameters
-- divMod returns a tuple with the division and modulo of its parameters

-----------------
-- Will they work
-----------------
-- 1. max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
--    Result: 5
--
-- 2. compare (3 * 4) (3 * 5)
--    Result: LT
--
-- 3. compare "Julie" True
--    Doesn't work
--
-- 4. (5 + 3) > (3 + 6)
--    Result: False

---------------
-- Eq Instances
---------------
-- 1.
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (TisAn x) == (TisAn x') = x == x'

-- 2.
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (Two x y) == (Two x' y') = (x == x') && (y == y')

-- 3.
data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (TisAnInt i) == (TisAnInt i') = i == i'
  (TisAString s) == (TisAString s') = s == s'
  _ == _ = False

-- 4.
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (Pair x y) == (Pair x' y') = (x == x') && (y == y')

-- 5.
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple x y) == (Tuple x' y') = (x == x') && (y == y')

-- 6.
data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne x) == (ThisOne x') = x == x'
  (ThatOne x) == (ThatOne x') = x == x'
  _ == _ = False

-- 7.
data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello x) == (Hello x') = x == x'
  (Goodbye x) == (Goodbye x') = x == x'
  _ == _ = False
