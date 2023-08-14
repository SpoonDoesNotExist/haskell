module TypeClasses where

-- class Eq a where
--   (==), (/=) :: a -> a -> Bool

-- definition by default
-- minimal complete definition
--   x /= y = not (x == y)
--   x == y = not (x /= y)

-- instance Eq Bool where
--   True == True = True
--   False == False = True
--   _ == _ = False

-- instance (Eq a , Eq b) => Eq (a, b) where
--     p1 == p2 = fst p1 == fst p2 && snd p1 == snd p2
--     (/=) p1 p2 = False

-- Printable type class
-- Type class for printing type values
class Printable a where
  toString :: a -> String

-- instantiate Bool from Printable
instance Printable Bool where
  toString a
    | a = "true"
    | otherwise = "false"

instance Printable () where
  toString a = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

--
