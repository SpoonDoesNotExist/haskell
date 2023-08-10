import Data.Char

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

-- Two digits to Int

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then 10 * digitToInt x + digitToInt y else 100

-- 2D points distance

coordinateDiff :: (Double, Double) -> (Double, Double) -> [Double]
coordinateDiff point1 point2 = [fst point1 - fst point2, snd point1 - snd point2]

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ sum $ map (^ 2) $ coordinateDiff p1 p2