{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chapter07.Content.Common where

-- See Chapter07/Content/Constant.hs
-- See Chapter07/Content/Function.hs

myNum = 1

myVal f = f + myNum

stillAFunction a b c = a ++ b ++ c

addOne :: Integer -> Integer
addOne x = x + 1

-- See Chapter07/Content/BindExp1.hs
-- See Chapter07/Content/BindExp2.hs
-- See Chapter07/Content/BindExp3.hs

triple :: Integer -> Integer
triple x = x * 3

-- See Chapter07/Content/ItIsTwo1.hs
-- See Chapter07/Content/ItIsTwo2.hs
-- See Chapter07/Content/ItIsTwo3.hs

-- See Chapter07/Content/RegisteredUser1.hs
-- See Chapter07/Content/RegisteredUser2.hs

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin
  = Peng WherePenguinsLive
  deriving (Eq, Show)

-- is it South Africa? If so, return True
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

{-# ANN module "HLint: ignore Redundant bracket" #-}
-- in this final function, the || operator
-- is an `or` function, which will return True
-- if either value is True
antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- See Chapter07/Content/TupleFunctions.hs

{-# ANN module "HLint: ignore Use if" #-}
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

-- See Chapter07/Content/GreetIfCool3.hs

{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Redundant lambda" #-}
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

-- See Chapter07/Content/ReturnBroken.hs

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

-- See Chapter07/Content/HigherOrder1.hs
-- See Chapter07/Content/HigherOrder2.hs

myAbs' :: Integer -> Integer
myAbs' x = if x < 0 then (-x) else x

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

-- c is the hypotenuse of the triangle. Google it.

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "not right"

dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | x <= 4    = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- See Chapter07/Content/Arith2.hs
