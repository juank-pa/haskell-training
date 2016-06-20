module Chapter11.Content.Common where

---------------------------
-- Data declarations review
---------------------------
data Trivial = Trivial'
data UnaryTypeCon a = UnaryValueCon a

-----------------------------
-- Data and type constructors
-----------------------------
data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData
-- no witness to the contrary ^

-- This will work because the value 10 agrees
-- with the type variable being bound to Int
myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- This will not work because 10
-- cannot be reconciled with the
-- type variable being bound to String
badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux 10

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

-- What's a type and what's data?
data Price
  = Price Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

{-# ANN module "HLint: ignore Use camelCase" #-}
data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

-- Data constructor arities
-- nullary
data Example0
  = Example0 deriving (Eq, Show)
-- unary
data Example1
  = Example1 Int deriving (Eq, Show)
-- product of Int and String
data Example2
 = Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)

-- What makes these datatypes algebraic?
data Example = MakeExample deriving Show

data Goats = Goats Int deriving (Eq, Show)

tooManyGoats' :: Int -> Bool
tooManyGoats' n = n > 42

newtype Goats
  = Goats Int deriving (Eq, Show)
newtype Cows
  = Cows Int deriving (Eq, Show)

-- Now we can rewrite our type to be
-- safer, pattern matching in order
-- to access the Int inside our data
-- constructor Goats.
tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

instance TooMany Goats where
  tooMany (Goats n) = tooMany n

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- Product types
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

data TwoQs
  = MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

type TwoQs = (QuantumBool, QuantumBool)

data Person = MkPerson String Int deriving (Eq, Show)


-----------------------------
-- Higher kinded data types
-----------------------------
data Silly a b c d = MkSilly a b c d deriving Show
