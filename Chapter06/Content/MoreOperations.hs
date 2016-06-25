module Chapter06.Content.MoreOperations where

add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x
