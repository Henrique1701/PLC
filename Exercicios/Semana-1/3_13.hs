maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
 | (x >= y) && (x >= z) = x
 | (y >= z) = y
 | otherwise  = z