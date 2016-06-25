{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- arith3broken.hs
module Chapter05.Exercises.Arith3Broken where

main :: IO ()
main = do
  -- Fixed. It was: print 1 + 2
  print $ 1 + 2
  -- Fixed. It was: puStrLn 10
  putStrLn "10"
  -- Fixed. It was: print (negate -1)
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1
