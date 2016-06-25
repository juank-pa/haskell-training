module Chapter06.Content.Common where

subtractThenAdd :: Num a => a -> a -> a
subtractThenAdd x y = (x - y) + 1

divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

-- This is even worse than the last one.
-- Don't use typeclasses to define default values.
-- Seriously. Haskell Ninjas will find you
-- and replace your toothpaste with muddy chalk.
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

-- pretend newtype is data for now
newtype Age
  = Age Integer
  deriving (Eq, Show)

{-# ANN module "HLint: ignore Eta reduce" #-}
instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year
 = Year Integer
 deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

-- See Chapter06/Content/Trivial1.hs
-- See Chapter06/Content/Trivial2.hs
-- See Chapter06/Content/DayOfWeek.hs

f :: Int -> Bool
f 1 = True

f2 :: Int -> Bool
f2 1 = True
f2 2 = True

-- This will compile without complaint
-- and is not partial.
f3 :: Int -> Bool
f3 1 = True
f3 2 = True
f3 3 = True
f3 _ = False

-- See Chapter06/Content/Identity1.hs
-- See Chapter06/Content/Identity2.hs
-- See Chapter06/Content/DayOfWeekOrd.hs
-- See Chapter06/Content/DayOfWeekOrd2.hs
-- See Chapter06/Content/MoreOperationsWrong.hs
-- See Chapter06/Content/MoreOperations.hs
-- See Chapter06/Content/OrdImpliesEq1.hs
-- See Chapter06/Content/OrdImpliesEq2.hs
-- See Chapter06/Content/ConcreteImpliesAll.hs
