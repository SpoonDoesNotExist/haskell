module Functions where
-- Len of 3d vector
square x = x * x
sumTwo x y = x + y

sumOfSquares3 x y z = sumTwo (square x) (sumTwo (square y) (square z) )
lenVec3 x y z = sqrt (sumOfSquares3 x y z)

-- Sign of number
sgn x = if x>0 then 1 else -1
sign x = if x==0 then 0 else sgn x

-- Language translation
let translate languageTo languageFrom text = ???
let translateFromSpanishToRussian = translate "Russian" "Spanish"
let translateFromEnglishToRussian = translate "Russian" "English"
let translateToRussian = translate "Russian"