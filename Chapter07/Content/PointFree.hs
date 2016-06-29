{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chapter07.Content.PointFree where

-- pointfree versions of the above
blah = id
addAndDrop = const . (1 +)
reverseMkTuple = flip (,)
reverseTuple = uncurry (flip (,))
-- The following doesn't compile
--wtf = zipWith (+) . (join .) . map
