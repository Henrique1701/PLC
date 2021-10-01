threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m == n) && (m == p)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p r =
 (threeEqual m n p) && (m == r)