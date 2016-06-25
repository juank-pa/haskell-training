{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Chapter05.Content.Common where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

{-# ANN module "HLint: ignore Redundant bracket" #-}
typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)

{-# ANN module "HLint: ignore Redundant lambda" #-}
anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

{-# ANN module "HLint: ignore Collapse lambdas" #-}
anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested =
  \i -> \b -> i + (nonsense b)

-- See Chapter05/Content/TypeInference1.hs
-- See Chapter05/Content/TypeInference2.hs

{-# ANN module "HLint: ignore Eta reduce" #-}
triple :: Integer -> Integer
triple x = tripleItYo x
  where tripleItYo :: Integer -> Integer
        tripleItYo y = y * 3
