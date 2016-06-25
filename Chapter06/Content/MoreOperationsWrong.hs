module Chapter06.Content.MoreOperationsWrong where

add :: a -> a -> a
add x y = x + y

addWeird :: Num a => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x
