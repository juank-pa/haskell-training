module Exercises where

-- Reading syntax
-- 1.
-- a) Right
-- b) (++) [1,2,3] [4,5,6]
-- c) Right
-- d) "hello" ++ " world"
-- e) "hello" !! 4
-- f) Right
-- g) take 4 "lovely"
-- h) Right

-- 2.
-- a) -> d)
-- b) -> c)
-- c) -> e)
-- d) -> a)
-- e) -> b)

-- Building functions
-- 1.
-- a) "Curry is awesome" ++ "!"
-- b) ("Curry is awesome!" !! 4):""
-- c) drop 9 "Curry is awesome!"

-- 2.
exclaim :: String -> String
exclaim s = s ++ "!"

fourthLetter :: String -> String
fourthLetter s = (s !! 4):""

dropNine :: String -> String
dropNine = drop 9

-- 3.
thirdLetter :: String -> Char
thirdLetter = (!! 2)

-- 4.
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- 5.
rvrs :: String -> String
rvrs s = drop 9 s ++ " " ++ take 2 (drop 6 s) ++ " " ++ take 5 s

-- 6.
main :: IO ()
main = print $ rvrs "Curry is awesome"
