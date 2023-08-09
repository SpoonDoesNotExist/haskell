module Operators where

-- Sum of squares
infixl 6 *+*
(*+*) x y = x ^ 2 + y ^ 2

-- Abs difference
x |-| y = max (x-y) (y-x)

-- Partial application of an operator
(`mod` 14) ((+ 5) 10)