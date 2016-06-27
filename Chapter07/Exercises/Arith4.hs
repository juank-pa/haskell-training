{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- arith4.hs
module Chapter07.Exercises.Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTripPF' :: (Show a, Read b) => a -> b
roundTripPF' = read . show

{-# ANN module "HLint: ignore Evaluate" #-}
main = do
  print (roundTrip 4)
  print (roundTripPF 4)
  print (roundTripPF' (3 :: Int) :: Double)
  print (id 4)
