-- Intermission.hs
module Intermission where

import Test.Hspec
import Data.Function (on)

-- New version supporting negative values
mult :: (Num a, Eq a, Ord a) => a -> a -> a
mult _ 0 = 0
mult x y
  | y > 0     = x + mult x (y - 1)
  | otherwise = (mult `on` negate) x y

main :: IO ()
main = hspec $
  describe "mult" $ do
    it "mult 3 5 is equal to 15" $
      mult 3 5 `shouldBe` 15
    it "mult (-3) 5 is equal to (-15)" $
      mult (-3) 5 `shouldBe` (-15)
    it "mult 3 (-5) is equal to (-15)" $
      mult 3 (-5) `shouldBe` (-15)
    it "mult (-3) (-5) is equal to (-15)" $
      mult (-3) (-5) `shouldBe` 15
