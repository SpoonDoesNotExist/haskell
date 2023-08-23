module ListGen where

-- Fibonacci series generator
fibStream :: [Integer]
fibStream = 0 : zipWith (+) fibStream (1 : fibStream)

-- List comperhantion
-- Generate change using specific coins

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change total
  | total == 0 = [[]]
  | total < 0 = []
  | otherwise = [x : y | x <- coins, y <- change (total - x)]