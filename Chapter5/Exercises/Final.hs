module Chapter5.Exercises.Final where

-- Mutiple choice
-- 1. c)
-- 2. a)
-- 3. b)


-- Determine the type
-- 1.
-- a) 54             :: Num a => a
-- b) (0,"dodge")    :: Num a => (a,[Char])
-- c) (0,"dodge")    :: (Integer,[Char])
-- d) False          :: Bool
-- e) 5              :: Int
-- f) False          :: Bool
-- 2. Num a => a
-- 3. Num a => a -> a
-- 4. Fractional a => a
-- 5. [Char]

-- Does it compile
-- 1. Doesn't compile. Possible fix.
--    bigNum = (^) 5 $ 10
--    wahoo = bigNum
-- 2. Compiles
-- 3. Doesn't compile. Possible fix.
--    a = (+)
--    b = 5
--    c = a 10
--    d = c 200
-- 4. Doesn't compile. Possible fix.
--    a = 12 + b
--    b = 1000 * c
--    c = 10

-- Type variable or specific type constructor
-- 1.
-- 2.
--   fully polymorphic [0]
--   concrete [1], [2]
-- 3.
--   fully polymorphic [0]
--   constrained polymorphic Enum [1]
--   concrete [2]
-- 4.
--   fully polymorphic [0], [1]
--   concrete [2]

-- Write a type signature
-- 1. functionH :: [a] -> a
-- 2. functionC :: Ord a => a -> a -> Bool
-- 3. functionS :: (a, b) -> b

-- Given a type, write the function
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
a' :: (a -> b) -> a -> b
a' f' x = f' x
-- or
a'' :: (a -> b) -> a -> b
a'' f' = f'

-- Fix it
-- 1. see Sing.hs
-- 2. Just chage the > operator with a <
-- 3. see Arith3Broken.hs

-- Type-Known-Do
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
