module Chapter11.Content.Construct where

-- Core declarations

data GuessWhat
  = Chickenbutt deriving (Eq, Show)

data Id a
  = MkId a deriving (Eq, Show)

data Product a b
  = Product a b deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b
  = RecordProduct { pfirst :: a
                  , psecond::b
                  }
                  deriving (Eq, Show)

-- Constructing types

newtype NumCow
  = NumCow Int
  deriving (Eq, Show)

newtype NumPig
  = NumPig Int
  deriving (Eq, Show)

-- Product of two types
data Farmhouse
  = Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep
  = NumSheep Int
  deriving (Eq, Show)

-- Product of three types
data BigFarmhouse
  = BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse'
  = Product NumCow (Product NumPig NumSheep)

-- Sums
type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo
  = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo
  = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo
  = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

-- Sum of three types
data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo

type Animal'
  = Sum CowInfo (Sum PigInfo SheepInfo)

-- Contructing Nullary values
trivialValue :: GuessWhat
trivialValue = Chickenbutt

-- Constructing Unary values
idInt :: Id Integer
idInt = MkId 10

{-# ANN module "HLint: ignore Use id" #-}
idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

-- Product types

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

-- Sum types

data Twitter
  = Twitter deriving (Eq, Show)

data AskFm
  = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

-- Sum types using type aliases

type Twitter2 = String
type AskFm2 = String

twitter :: Sum Twitter2 AskFm2
twitter = First "Twitter"

-- This is an error but the compiler won't complaint
askfm :: Sum Twitter2 AskFm2
askfm = First "AskFm"

-- Product with record syntax

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct 42 0.00001

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42
                         , psecond = 0.00001
                         }

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

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Haskell }

-- We can reorder stuff when we use record syntax
feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda , os = GnuPlusLinux }

-- Constructor partial application
data ThereYet =
  There Integer Float String Bool deriving (Eq, Show)

-- who needs a "builder pattern"?
nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yusssss :: ThereYet
yusssss = notQuite False
