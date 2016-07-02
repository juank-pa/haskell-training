{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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
--
-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

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

-- See Chapter11/Content/Goats1.hs
-- See Chapter11/Content/Goats2.hs
-- See Chapter11/Content/Goats3.hs
-- See Chapter11/Content/Goats4.hs
-- See Chapter11/Content/Goats5.hs

-- Product types
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

data TwoQs
  = MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

type TwoQsTuple = (QuantumBool, QuantumBool)

-- See Chapter11/Content/Person.hs
-- See Chapter11/Content/PersonRecord.hs

-- See Chapter11/Content/SumOfProducts.hs
-- See Chapter11/Content/ProductOfSums.hs
-- See Chapter11/Content/StricterSumOfProducts.hs

-- See Chapter11/Content/Construct.hs
-- See Chapter11/Content/Deconstruct.hs

-- See Chapter11/Content/ExponentialFunctionType.hs

-----------------------------
-- Higher kinded data types
-----------------------------
data Silly a b c d = MkSilly a b c d deriving Show

data Product a b
  = a :&: b
  deriving (Eq, Show)

-- See Chapter11/Content/BinaryTree.hs
