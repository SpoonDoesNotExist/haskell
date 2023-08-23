module RFold where

-- list concat using `foldr`
concatList :: [[a]] -> [a]
concatList = foldr (++) []

--
lengthList :: [a] -> Int
lengthList = foldr (\x ts -> 1 + ts) 0

--
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0