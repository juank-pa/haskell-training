{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Chapter05.Exercises.Final where

-----------------
-- Mutiple choice
-----------------
-- 1. A value of type [a] is   c) a list whose elements are all of some type 𝑎
-- 2. A function of type [[a]] -> [a] could   a) take a list of strings as an argument
-- 3. A function of type [a] -> Int -> a   b) returns one element of type 𝑎 from a list

---------------------
-- Determine the type
---------------------
-- 1.
-- a) (* 9) 6
--    54             :: Num a => a
--
-- b) head [(0,"doge"),(1,"kitteh")]
--    (0,"dodge")    :: Num a => (a,[Char])
--
-- c) head [(0 :: Integer ,"doge"),(1,"kitteh")]
--    (0,"dodge")    :: (Integer,[Char])
--
-- d) if False then True else False
--    False          :: Bool
--
-- e) length [1, 2, 3, 4, 5]
--    5              :: Int
--
-- f) (length [1, 2, 3, 4]) > (length "TACOCAT")
--    False          :: Bool
--
-- 2. Given:
--    x = 5
--    y = x + 5
--    w = y * 10
--
--    The type of w is: Num a => a
--
-- 3. Given:
--    x = 5
--    y = x + 5
--    z y = y * 10
--
--    The type of z is: Num a => a -> a
--
-- 4. Given:
--    x = 5
--    y = x + 5
--    f = 4 / y
--
--    The type of f is: Fractional a => a
--
-- 5. Given:
--    x = "Julie"
--    y = " <3 "
--    z = "Haskell"
--    f = x ++ y ++ z
--
--    The type of f is: [Char]

------------------
-- Does it compile
------------------
-- 1. bigNum = (^) 5 $ 10
--    wahoo = bigNum $ 1
--    doesn't compile. Possible fix.
--    bigNum = (^) 5 $ 10
--    wahoo = bigNum
--
-- 2. x = print
--    y = print "woohoo!"
--    z = x "hello world"
--    compiles
--
-- 3. a = (+)
--    b = 5
--    c = b 10
--    d = c 200
--    doesn't compile. Possible fix.
--    a = (+)
--    b = 5
--    c = a 10
--    d = c 200
--
-- 4. a = 12 + b
--    b = 10000 * c
--    doesn't compile. Possible fix.
--    a = 12 + b
--    b = 1000 * c
--    c = 10

---------------------------------------------
-- Type variable or specific type constructor
---------------------------------------------
-- 1. f :: Num a => a -> b -> Int -> Int
--    --           [0]  [1]   [2]    [3]
--    constrained polymorphic Num [0]
--    fully polymorphic [1]
--    concrete [2], [3]
--
-- 2. f :: zed -> Zed -> Blah
--    --   [0]    [1]    [2]
--    fully polymorphic [0]
--    concrete [1], [2]
--
-- 3. f :: Enum b => a -> b -> C
--    --            [0]  [1]  [2]
--    fully polymorphic [0]
--    constrained polymorphic Enum [1]
--    concrete [2]
--
-- 4. f :: f -> g -> C
--    --  [0]  [1]  [2]
--    fully polymorphic [0], [1]
--    concrete [2]

-------------------------
-- Write a type signature
-------------------------
-- Given the implementation write a type signature
-- 1.
functionH :: [a] -> a
functionH (x:_) = x
functionH     _ = error ""

-- 2.
{-# ANN module "HLint: ignore Redundant if" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y

-----------------------------------
-- Given a type, write the function
-----------------------------------
-- 1.
i :: a -> a
i x = x

-- 2.
c :: a -> b -> a
c x _ = x

-- 3. Yes, they are alpha equivalent
-- 4.
c' :: a -> b -> b
c' _ y = y

-- 5.
-- 6.
co :: (b -> c) -> (a -> b) -> (a -> c)
co f' g' x = f' (g' x)
--   or
co' :: (b -> c) -> (a -> b) -> (a -> c)
co' f' g' = f' . g'

-- 7.
a :: (a -> c) -> a -> a
a _ x = x

-- 8.
{-# ANN module "HLint: ignore Eta reduce" #-}
a' :: (a -> b) -> a -> b
a' f' x = f' x
-- or
a'' :: (a -> b) -> a -> b
a'' f' = f'

---------
-- Fix it
---------
-- 1. see Sing.hs
-- 2. Just chage the > operator with a <
-- 3. see Arith3Broken.hs

----------------
-- Type-Known-Do
----------------
-- 1.
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)

-- 2.
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)

-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f' g' x = fst (g' (f' x))
