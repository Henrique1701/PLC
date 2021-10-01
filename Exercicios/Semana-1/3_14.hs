minThree :: Int -> Int -> Int -> Int
minThree x y z = 
 if ((x < y) && (x < z)) then x 
 else if (y < z) then y
 else z