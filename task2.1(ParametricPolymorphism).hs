module ParametricPolymorphism where

import Data.Function

getSecondFrom :: t0 -> t1 -> t2 -> t1
getSecondFrom a b c = b

foo :: a -> a -> b -> a -> a
foo x1 x2 x3 x4 = x1

-- `on`
multSecond = g `on` h

g = (*)

h = snd

--
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

-- Composition
composition = f_ . g_ . h_

f_ = logBase 2

g_ = (^ 3)

h_ = max 42

-- curry
curry' :: ((a, b) -> t) -> a -> b -> t
curry' f x y = f (x, y)

uncurry' :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry' f (x, y) = f x y

-- Derivation of "Swap" function.
swap' :: (a, b) -> (b, a)
swap' = uncurry (flip (,))

-- (,) :: a -> b -> (a, b)
-- flip (,) :: b -> a -> (a, b)
-- uncurry flip (,) :: (a, b) -> (b, a)