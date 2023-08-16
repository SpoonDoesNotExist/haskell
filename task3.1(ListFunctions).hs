-- Add two elements to list
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a1 a2 elemList = a1 : a2 : elemList

-- Generate list of repeating elements of length `n`
nTimes :: a -> Int -> [a]
nTimes elem size = helper size
  where
    addElem = (elem :)
    helper size
      | size == 0 = []
      | otherwise = addElem $ helper (size - 1)

-- Filter even numbers
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) = if odd x then x : oddsOnly xs else oddsOnly xs

-- Detect palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == rev xs []
  where
    rev [] rxs = rxs
    rev (x : xs) rxs = rev xs (x : rxs)

-- Sum 3 lists elementwise
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = sum2 xs $ sum2 ys zs
  where
    sum2 (x : xs) (y : ys) = (x + y) : sum2 xs ys
    sum2 [] ys = ys
    sum2 xs [] = xs

-- Group equal elements if they are sequential
groupElems :: Eq a => [a] -> [[a]]
groupElems xs = helper xs [] []
  where
    helper :: Eq a => [a] -> [a] -> [[a]] -> [[a]]

    helper [] repeats result = result
    helper [x1] repeats result = reverse $ (x1 : repeats) : result
    helper (x1 : x2 : xs) repeats result =
      if x1 == x2
        then helper (x2 : xs) (x1 : repeats) result
        else helper (x2 : xs) [] ((x1 : repeats) : result)


groupElems2 [] = []
groupElems2 (x : xs) = equals : groupElems2 others
  where
    (equals, others) = span (== x) (x : xs)
