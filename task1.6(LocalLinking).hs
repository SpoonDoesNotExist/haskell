import Type.Reflection

-- Find roots of square equation using `let`
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
  let d = sqrt $ b ^ 2 - 4 * a * c
      aTwice = a * 2
      x1 = (-b - d) / aTwice
      x2 = (-b + d) / aTwice
   in (x1, x2)


-- Find roots of square equation using `where`
roots' a b c = (x1, x2)
  where
    d = sqrt $ b ^ 2 - 4 * a * c
    aTwice = a * 2
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice

rootsDiff a b c =
  let (x1, x2) = roots a b c
   in x2 - x1

-- Recurrent sequence: a0=1; a1=2; a2=3; a(k+3) = a(k+2) + a(k+1) - 2 * a(k)
seqA :: Int -> Int
seqA n =
  let helper 0 a0 a1 a2 = a0
      helper n a0 a1 a2 = helper (n - 1) a1 a2 (a2 + a1 - 2 * a0)
   in helper n 1 2 3

seqA' :: Int -> Int
seqA' n = helper n 1 2 3
  where
    helper 0 a0 a1 a2 = a0
    helper n a0 a1 a2 = helper (n - 1) a1 a2 (a2 + a1 - 2 * a0)

-- Sum and number of digits of the decimal notation of a given integer number
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x == 0 = (0, 1)
  | otherwise = helper 0 0 (x * sgn x)
  where
    sgn :: Integer -> Integer
    sgn x = if x > 0 then 1 else -1

    helper :: Integer -> Integer -> Integer -> (Integer, Integer)
    helper summ count 0 = (summ, count)
    helper summ count x = helper (summ + mod x 10) (count + 1) (div x 10)

-- Calculate integral with Trapezoidal rule
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = summator 0 start
  where
    start = a
    stop = b

    terminationCheck :: Double -> Double -> Bool
    terminationCheck = if start < stop then checkRight else checkLeft
      where
        checkRight :: Double -> Double -> Bool
        checkRight cur bound = cur >= bound

        checkLeft :: Double -> Double -> Bool
        checkLeft cur bound = cur <= bound

    step = (stop - start) / 10000.0

    trapezoidArea x1 x2 h = h * (f x1 + f x2) / 2

    summator summ cur
      | terminationCheck cur stop = summ
      | otherwise =
          let area = trapezoidArea cur (cur + step) step
           in summator (summ + area) (cur + step)

integration2 :: (Double -> Double) -> Double -> Double -> Double
integration2 f a b = step * (boundTrapezoid + summator 0 a stepCount)
  where
    stepCount = 1000000
    step = (b - a) / stepCount
    boundTrapezoid = (f a + f b) / 2

    summator summ cur 0 = summ
    summator summ cur remainSteps = summator (summ + f (cur + step)) (cur + step) (remainSteps - 1)
