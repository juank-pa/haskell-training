{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Chapter11.Exercises.DaPhone where

import Data.Char
import Data.List
import Data.Function

-- 1.
type Digit = Char
type Presses = Int
type Cost = Int

data DaPhone = DaPhone [(Digit, [Digit])]

phone :: DaPhone
phone = DaPhone [ ('1' ,"1")
                , ('2' ,"abc2")
                , ('3' ,"def3")
                , ('4' ,"ghi4")
                , ('5' ,"jkl5")
                , ('6' ,"mno6")
                , ('7' ,"pqrs7")
                , ('8' ,"tuv8")
                , ('9' ,"wxyz9")
                , ('*' ,"^*")
                , ('0' ," 0")
                , ('#' ,".,#")
                ]

-- 2.
convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps ph c = press ++ [findDigit ph c']
  where (c', press) = decap ph c

decap :: DaPhone -> Char -> (Char, [(Digit, Presses)])
decap ph c
  | isUpper c = (toLower c, [findDigit ph '^'])
  | otherwise = (c, [])

-- I suppose only valid data for helper functions
-- Determines the digit and amount of presses to get a character
findDigit :: DaPhone -> Char -> (Digit, Presses)
findDigit (DaPhone p) c = (key, presses val)
  where
    (key, val) = head . filter ((c `elem`) . snd) $ p
    presses   = ((+1) . head) . elemIndices c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph = concatMap (reverseTaps ph)

-- 3.
fingerTaps :: [(Char, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

-- 4.
-- The question is not very clear to me.
-- Is the most popular letter refering to the digit in the pad that is pressed
-- the most in a single sentence, or is it the final rendered letter?
-- If it is the final rendered letter, the "cost" must refer to the amount of
-- taps required to get to it, otherwise you would not use `fingerTaps` and
-- `reverseTaps` as the exersise expects.

-- Thus I made two different versions:
-- `mostPopularDigit` gets the digit in the phone pad that was pressed the most.
-- Doesn't matter which actual letter you were trying to get out of the presses,
-- all of them counted as part of the digits' "cost".
mostPopularDigit :: String -> Char
mostPopularDigit = listMostPopular fst digitAndCost . cellPhonesDead phone
  where digitAndCost e = (fst (head e), fingerTaps e)

-- This second version `mostPopularLetter` gets the final rendered letter
-- the has the highest "cost". This time the letter "cost" is the amount of taps
-- needed to get to render that letter trhoughout the whole string.
mostPopularLetter :: String -> Char
mostPopularLetter = listMostPopular id letterAndCost . map (reverseTaps phone)
  where letterAndCost e = (taps phone (head e), fingerTaps . concat $ e)

-- `maximumBy` could have been replaced by a `foldr` with which
-- we are more confortable by now, e.g.:
-- foldr (\a@(_,y) b@(_,y') -> if y > y' then a else b) ('',0))
-- foldr (\a b -> if snd a > snd b then a else b) ('',0)
-- foldr (\a b -> bool b a (snd a > snd b)) ('',0)
--
-- Also:
-- (compare `on` snd) == (\a b -> compare (snd a) (snd b))

-- Abstraction to simplify the process of getting the most popular element
-- given a data list.
-- It receives:
--   * (a -> b) : A function that received every element on the list and
--     converts it to an Ord instance to sort and group the list.
--   * ([a] -> (c, Cost)) : A function that takes each grouped list, comming
--     from the previous function parameter and converts it to a tuple with
--     a result element and its respective cost.
--   * [a] : A list from which to determine a popular element.
-- It returns:
--   * c : The most popular element based on the calculated cost
listMostPopular :: Ord b => (a -> b) -> ([a] -> (c, Cost)) -> [a] -> c
listMostPopular f g = maxCostItem . costList . groupedList
  where
    maxCostItem = fst . maximumBy (compare `on` snd)
    groupedList = groupBy ((==) `on` f) . sortOn f
    costList = map g

-- This simulates taps in the phone to get back the rendered letter.
taps :: DaPhone -> [(Digit, Presses)] -> Char
taps ph [_,d'] = toUpper $ findLetter ph d'
taps ph [d]    = findLetter ph d

-- Finds the rendered char for a digit press.
findLetter :: DaPhone -> (Digit, Presses) -> Char
findLetter (DaPhone pad) (d,p) = (!!(p - 1)). snd . head . filter ((== d) . fst) $ pad

-- 5.
-- From here on I suppose `mostPopularLetter` is the right answer
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = listMostPopular fst wordAndCost . map wordAndTaps . concatMap words
  where
    wordAndTaps x = (x, cellPhonesDead phone x)
    wordAndCost e = (fst (head e), foldr ((+) . fingerTaps . snd) 0 e)

