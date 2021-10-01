threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = (m /= n) && (m /= p) && (n /= p)

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m == n) && (m == p)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p r =
 (threeEqual m n p) && (m == r)

mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not ((m == n) && (n == p))

-- threeEqual  (2+3) 5 (11 `div` 2)
-- mystery (2+4) 5 (11 `div` 2)
-- threeDifferent (2+4) 5 (11 `div` 2) 
-- fourEqual (2+3) 5 (11 `div` 2) (21 `mod` 11)

isEqual :: (Eq t) => t -> t -> Bool
isEqual x y = (x == y)

maior :: Int -> Int -> Int
maior x y
 | x > y = x
 | y > x = y
 | otherwise = 0