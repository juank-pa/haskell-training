module Chapter03.Exercises.Final where

-----------------
-- Reading syntax
-----------------
-- 1. Fix if necessary
-- a) concat [[1, 2, 3], [4, 5, 6]] -- is right
-- b) ++ [1, 2, 3] [4, 5, 6]
--    (++) [1,2,3] [4,5,6] -- fix
-- c) (++) "hello" " world" -- is right
-- d) ["hello" ++ " world]
--    "hello" ++ " world" -- fix
-- e) 4 !! "hello"
--    "hello" !! 4 -- fix
-- f) (!!) "hello" 4 -- is right
-- g) take "4 lovely"
--    take 4 "lovely" -- fix
-- h) take 3 "awesome" -- is right

-- 2.
-- a) -> d)
--    concat [[1 * 6], [2 * 6], [3 * 6]] -- expr
--    [6,12,18] -- result
-- b) -> c)
--    "rain" ++ drop 2 "elbow" -- expr
--    "rainbow" -- result
-- c) -> e)
--    10 * head [1, 2, 3] -- expr
--    10 -- result
-- d) -> a)
--    (take 3 "Julie") ++ (tail "yes") -- expr
--    "Jules" -- result
-- e) -> b)
--    concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]] -- expr
--    [2,3,5,6,8,9] -- result

---------------------
-- Building functions
---------------------
-- 1.
-- a) "Curry is awesome" ++ "!"
--    -- Given
--    -- "Curry is awesome"
--    -- Return
--    -- "Curry is awesome!"
-- b) ("Curry is awesome!" !! 4):""
--    -- Given
--    "Curry is awesome!"
--    -- Return
--    "y"
-- c) drop 9 "Curry is awesome!"
--    -- Given
--    "Curry is awesome!"
--    -- Return
--    "awesome!"

-- 2. Write functions for the prevous expressions
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

-- 5. Reverse words in "Curry is awesome" with learnt functions only
rvrs :: String -> String
rvrs s = drop 9 s ++ " " ++ take 2 (drop 6 s) ++ " " ++ take 5 s

-- 6. See Chapter3/Exercises/Reverse.hs
