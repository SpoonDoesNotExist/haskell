module Recursion where

factorial n = if n == 0 then 1 else n * factorial (n - 1)

-- Pattern matching
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- Double factorial
dFactorial :: Integer -> Integer
dFactorial 0 = 1
dFactorial 1 = 1
dFactorial n = n * dFactorial (n - 2)

-- Exceptions
factorial'' 0 = 1
factorial'' n = if n < 0 then error "`n` must be non negative" else n * factorial'' (n - 1)

-- Guards
factorial''' n
  | n == 0 = 1
  | n < 0 = error "`n` must be non negative"
  | otherwise = n * factorial''' (n - 1)

-- Fibonacci bidirectional
fibonacciBi :: Integer -> Integer
fibonacciBi n
  | n > 1 = fibonacciBi (n - 1) + fibonacciBi (n - 2)
  | n < 0 = fibonacciBi (n + 2) - fibonacciBi (n + 1)
  | otherwise = n

-- Fibonacci with accumulation
fibHelper :: Integer -> Integer -> Integer -> Integer -> Integer
fibHelper n step sum1 sum2
  | n == step = sum1
  | n > 1 = fibHelper n (step + 2) (sum1 + sum2 + sum1) (sum1 + sum2)
  | n < 0 = fibHelper n (step - 2) (sum1 + sum1 - sum2) (sum2 - sum1)
  | otherwise = n

fibonacci :: Integer -> Integer
fibonacci n
  | even n = fibHelper n 0 0 1
  | n < 0 = fibHelper n 1 1 1
  | otherwise = fibHelper n 1 1 0

helper2 n s1 s2
  | n == 0 = s1
  | n == 1 = s2
  | n > 1 = helper2 (n - 1) s1 (s1 + s2)
  | n < 0 = helper2 (n + 1) (s2 - s1) s1

fibonacci2 :: Integer -> Integer
fibonacci2 n = helper2 n 0 1

-- Perfomance difference on M1/16
-- ghci> fibonacciBi 35
-- 9227465
-- (18.59 secs, 11,772,645,904 bytes)
-- ghci> fibonacci 35
-- 9227465
-- (0.01 secs, 96,672 bytes)