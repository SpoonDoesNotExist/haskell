-- This version accumulates sequence of calculations in `acc`
factorial :: Integer -> Integer
factorial n = helper 1 n
  where
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n - 1)

-- This version explicitly calls calculation of `acc * n` before doing a recursion step
factorial' :: Integer -> Integer
factorial' n = helper 1 n
  where
    helper acc 0 = acc
    helper acc n = (helper $! (acc * n)) (n - 1)
