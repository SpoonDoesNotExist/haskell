-- example of multiple instantiation
class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab x
    | doesEnrageGork x && doesEnrageMork x = stomp (stab x)
    | doesEnrageGork x && not (doesEnrageMork x) = stab x
    | not (doesEnrageGork x) && doesEnrageMork x = stomp x
    | otherwise = x

--
-- Inherit some class types to define SafeEnum with cyclic iteration

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a
    | a == maxBound = minBound
    | otherwise = succ a

  spred :: a -> a
  spred a
    | a == minBound = maxBound
    | otherwise = pred a

-- Average calculation with type changing
avg :: Int -> Int -> Int -> Double
avg x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3.0