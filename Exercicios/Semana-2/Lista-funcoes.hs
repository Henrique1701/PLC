{- Questão 1 -}
dobro :: Integer -> Integer
dobro x = 2 * x
{-------------}

{- Questão 2 -}
quadruplo :: Integer -> Integer
quadruplo x = (dobro x) + (dobro x)
{-------------}

{- Questão 3 -}
poli2 :: (Double, Double, Double, Double) -> Double
poli2 (a, b, c, x) = a * x^2 + b * x + c
{-------------}

{- Questão 4 -}
parImpar :: Int -> String
parImpar x
 | x `mod` 2 == 0 = "par"
 | otherwise = "impar"
{-------------}

{- Questão 5 -}
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
 | (x >= y) && (x >= z) = x
 | (y >= z) = y
 | otherwise  = z

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
--maxFour m n p q
-- | m >= maxThree n p q = m
-- | otherwise = maxThree n p q

--maxFour m n p q
-- | max m n >= max p q = max m n
-- | otherwise = max p q

maxFour m n p q
 | max m n >= maxThree n p q
  = max m n
 | otherwise = maxThree n p q
{-------------}

{- Questão 6 -}
quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais x y z
 | (x == y) && (x == z) = 3
 | (x == y) || (x == z) || (y == z) = 2
 | otherwise = 0
{-------------}

{- Questão 7 -}
ehZero :: Integer -> Bool
ehZero 0 = True
ehZero x = False
{-------------}

{- Questão 8 -}
tailSumTo :: Integer -> Integer -> Integer
tailSumTo 1 y = y + 1
tailSumTo x y = tailSumTo (x-1) (y+x)

sumTo :: Integer -> Integer
sumTo x = tailSumTo x 0
{-------------}

{- Questão 9 -}
tailPotencia :: Integer -> Integer -> Integer -> Integer
tailPotencia n k aux
 | k == 0    = aux
 | otherwise = tailPotencia (n) (k-1) (n*aux)

potencia :: Integer -> Integer -> Integer
potencia n k = tailPotencia n k 1
{-------------}

{- Questão 10 -}
coeficienteBinomial :: Integer -> Integer -> Integer
coeficienteBinomial n 0 = 1
coeficienteBinomial 0 k = 0
coeficienteBinomial n k = (coeficienteBinomial (n-1) k) + (coeficienteBinomial (n-1) (k-1))
{-------------}

{- Questão 11 -}
tailTribonacci :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
tailTribonacci parada anterior2 anterior1 acumulador indece
 | indece == parada = acumulador
 | otherwise = tailTribonacci
                             (parada) 
                             (anterior1)  
                             (acumulador) 
                             (anterior2 + anterior1 + acumulador)
                             (indece + 1)

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci x = tailTribonacci (x) (1) (2) (4) (4)
{-------------}

{- Questão 12 -}
addEspacosAux :: Int -> String -> String
addEspacosAux 0 s = s
addEspacosAux x s = addEspacosAux (x-1) (s ++ " ")

addEspacos :: Int -> String
addEspacos x = addEspacosAux x ""
{-------------}

{- Questão 13 -}
paraDireita :: Int -> String -> String
paraDireita x s = addEspacos x ++ s
{-------------}

{- Questão x -}

{-------------}