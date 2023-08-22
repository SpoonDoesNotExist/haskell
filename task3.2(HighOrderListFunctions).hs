module HOLF where

import Data.Char (isDigit, isUpper, isLower)

-- Split string of sequential digits and letters after it (like "243addfg") on number and letters ("243", "addfg")
readDigits :: String -> (String, String)
readDigits = span isDigit

-- Filter disjoint. Returns a list of elements that satisfy at least one of the predicates
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> p1 x || p2 x)

-- Hoar sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = sortedLess ++ (x : sortedGreater)
  where
    sortedLess = qsort $! filter (< x) xs
    sortedGreater = qsort $! filter (>= x) xs

-- Return squares and qubes of passed elements
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

-- Calculate all permutations for unique items
perms :: [Int] -> [[Int]]
perms [] = [[]]
perms [x] = [[x]]
perms (x : xs) = concatMap (elemPerms x) (perms xs)
  where
    elemPerms :: Int -> [Int] -> [[Int]]
    elemPerms x xs = helper x (length xs) []
      where
        helper :: Int -> Int -> [[Int]] -> [[Int]]
        helper x (-1) res = res
        helper x n res = helper x (n - 1) ((elemInsert x n xs) : res)

        elemInsert :: Int -> Int -> [Int] -> [Int]
        elemInsert x n xs = take n xs ++ [x] ++ drop n xs

-- Reverse words 
revWords :: String -> String
revWords = unwords . map reverse . words

-- Delete upper-cased words
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

-- Max of triplet for 3 lists
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 maxOfT
  where 
    maxOfT x y z = max x $ max y z