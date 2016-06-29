{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chapter07.Content.NotPointFree where

-- not pointfree
blah x = x
addAndDrop x y = x + 1
reverseMkTuple a b = (b, a)
reverseTuple (a, b) = (b, a)
-- The following doesn't compile
-- wtf d = zipWith (+) (\ l -> (map d l) >>= \ h -> h)
