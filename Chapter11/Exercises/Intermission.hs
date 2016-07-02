{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NegativeLiterals #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter11.Exercises.Intermission where

import Data.Int
import Data.Function
import Data.List
import Data.Bool

------------
-- Dog Types
------------
-- Given

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

-- 1. `Doggies` is a type constructor
-- 2. The kind of `Doggies` is * -> *
-- 3. The kind of `Doggies String` is *
-- 4. The type of `Husky 10` is `Num a => Doggies a`
-- 5. The type of `Husky (10 :: Integer)` is `Doggies Integer`
-- 6. The type of `Mastiff "Scooby Doo"` is `Doggies [Char]`
-- 7. `DogueDeBordeaux` is both, a type and a data constructor, depending on where
--    it's used..
-- 8. The type of `DogueDeBordeaux` is `doge -> DogueDeBordeaux doge`
-- 9. The type of `DogueDeBordeaux "doggie!"` is `DogueDeBordeaux [Char]`

-----------
-- Vehicles
-----------
-- Given

data Price = Price Integer deriving (Eq, Show)

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
  | Plane Airline Double
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. The type of `myCar` is `Vehicle`
-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4. If `getManu` is used with `Plane`, it will throw a run-time exception
-- 5. The previous code has already added a new size data to the Plane constructor.

--------------
-- Cardinality
--------------
-- 1. data PugType = PugData -- cardinality is 1
-- 2. Airline
--      = PapuAir
--      | CatapultsR'Us
--      | TakeYourChancesUnited -- cardinality is 3
-- 3. `Int16` cardinality is 65536
-- 4. The cardinality of `Int` is 18446744073709551616 (might depend on platform)
--    The cardinality of `Integer` is infinite.
--    `Integer` is not `Bounded` so you can't use `maxBound` and `minBound` on it.
-- 5. The 8 in `Int8` means 8 bits.
--    The amount of values representable with eight bits is: 2 ^ 8 = 256

--------------
-- For example
--------------
data Example = MakeExample deriving Show
-- 1. The type of 'MakeExample' is `Example`.
--    If you request the type of `Example` you get a "not in scope" error.
-- 2. Using :info on `Example` we can see it implements `Show`
-- 3. `:info Example2` shows `Int -> Example2` because it now needs one argument
--    of type `Int` to construct a value of type `Example2`.
data Example2 = MakeExample2 Int deriving Show

--------------
-- Logic Goats
--------------
-- 1.
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany x = x > 42

instance TooMany (Int, String) where
  tooMany (x, _) = tooMany x

-- 2.
instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

-- 3. Surrounded tuple in a newtype to prevent collisions with `(Int, Int)`
--    Only `Tuple (Int, Int)` is valid because `Int` is the only `Num` that has
--    an instance of `TooMany`.
newtype Tuple a = Tuple (a, a)

instance (Num a, TooMany a) => TooMany (Tuple a) where
  tooMany (Tuple (x, y)) = tooMany (x + y)

----------------
-- Pity the Bool
----------------
-- 1. The cardinality of the following is:
--    Big Bool + Small Bool = ?
--        2    +       2    = ?
--        2    +       2    = 4
--
--    Remember the cardinality of a Unary is the same as the contained data cardinality.
data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)

-- 2. The cardinality of `NumberOrBool` is:
--    Numba Int8 + BoolyBool Bool = ?
--          256  +           2    = ?
--          256  +           2    = 258
--
--    When trying to create Numba with numbers out of range you get an out of range warning.
--    Numbers inside the range work fine.

data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

myNumba = Numba (-128)

---------
-- Jammin
---------
data Fruit
  = Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

-- data JamJars
--  = Jam Fruit Int deriving (Eq, Show)

-- 1. I'm using this same module instead of creating a Jasmin one.
-- 2. Rewritten in record syntax
data JamJars = Jam
             { fruit :: Fruit
             , jars  :: Int
             }
             deriving (Eq, Show, Ord)
-- 3. `JamJars` cardinality:
--    Fruit * Int                  = ?
--    4     * 18446744073709551616 = ?
--    4     * 18446744073709551616 = 18446744073709551620
-- 4. Added `Ord` instance above.
-- 5.
row1 = Jam Peach 10
row2 = Jam Apple 21
row3 = Jam Blackberry 8
row4 = Jam Plum 14
row5 = Jam Peach 65
row6 = Jam Plum 2

allJam = [row1, row2, row3, row4, row5, row6]
allJamCount = map jars allJam

-- 6.
sumJamJars :: [JamJars] -> Int
sumJamJars = sum . map jars

-- 7. The exercise proposes a "function" without parameters but that would be a constant
maxJar :: JamJars -> JamJars -> JamJars
maxJar j j' = bool j j' (jars j' > jars j)

mostRow :: [JamJars] -> JamJars
mostRow (x:xs) = foldr f x xs
  where f x' y' = bool y' x' (jars x' > jars y')
mostRow _      = error "Empty list"

-- There are other possible solutions.

-- Using `maximumBy` from `Data.List` you can use a compare function.
-- I also used the `on` function from `Data.Function`.
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- It applies (a -> b) to both parameters a, before applying them to (b -> b -> c).
-- In our case: (compare `on` jars) a b == compare (jars a) (jars b)
mostRow' :: [JamJars] -> JamJars
mostRow' = maximumBy (compare `on` jars)

-- 8. 9.
compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

-- Using the suggested `compareKind` helper function
sortJamJars :: [JamJars] -> [JamJars]
sortJamJars = sortBy compareKind

-- Using `on` we do not need the helper function `compareKind`
sortJamJars' :: [JamJars] -> [JamJars]
sortJamJars' = sortBy (compare `on` fruit)

-- 10.
-- Using `compareKind`
groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\x y -> compareKind x y == EQ) . sortJamJars

groupJam' :: [JamJars] -> [[JamJars]]
groupJam' = groupBy ((==) `on` fruit) . sortJamJars'

-----------------------------
-- How Does Your Garden Grow?
-----------------------------
-- 1.
-- data FlowerType
--   = Gardenia
--   | Daisy
--   | Rose
--   | Lilac
--   deriving Show
--
-- type Gardener = String
--
-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show
--
-- Normal form:
type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy    Gardener
  | Rose     Gardener
  | Lilac    Gardener
  deriving Show

--------------
-- Programmers
--------------
-- Create a list of all possible programmers
data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os   :: OperatingSystem
             , lang :: ProgrammingLanguage
             }
             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- using list comprehension
allProgrammers :: [Programmer]
allProgrammers = [Programmer o l | o <- allOperatingSystems, l <- allLanguages]

-- or using concatMap
allProgrammers' :: [Programmer]
allProgrammers' = concatMap (\x -> map (Programmer x) allLanguages) allOperatingSystems

-- You can also solve this with Applicative Functors but this is beyond the Chapter's scope.

-----------
-- The Quad
-----------
-- Determine unique inhabitants for the following types
data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- 1. eQuad :: Either Quad Quad -- 4 + 4 = 8
-- 2. prodQuad :: (Quad, Quad) -- 4 * 4 = 16
-- 3. funcQuad :: Quad -> Quad -- 4 ^ 4 = 256
-- 4. gTwo :: Bool -> Bool -> Bool -- (2 ^ 2) ^ 2 = 2 ^ (2 * 2) = 16
-- 5. fTwo :: Bool -> Quad -> Quad -- (4 ^ 4) ^ 2 = 4 ^ (4 * 2) = 65536
