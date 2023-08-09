module Operators where

-- Sum of squares
infixl 6 *+*
(*+*) x y = x ^ 2 + y ^ 2

-- Abs difference
x |-| y = max (x-y) (y-x)

-- Partial application of an operator
(`mod` 14) ((+ 5) 10)

-- Operation priority
func $ x = func x

sin (pi / 2)
sin $ pi / 2
-- 
logBase 4 (min 20 (9 + 7))
logBase 4 $ min 20 $ 9 + 7