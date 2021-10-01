{- Quebra para resto 7 0 -}
resto :: Int -> Int -> Int
resto m n
 | m < n = m
 | otherwise = resto (m-n) n

{- Quebra para divide 4 (-4) -}
divide :: Int -> Int -> Int
divide m n
 | m < n = 0
 | otherwise = 1 + divide(m-n) n

{- Recursão mútua -}
ehPar, ehImpar :: Int -> Bool
ehImpar n
 | n<=0 = False
 | otherwise = ehPar (n-1)

ehPar n
 | n < 0 = False
 | n == 0 = True
 | otherwise = ehImpar (n-1)

vendas 0 = 5
vendas 1 = 7
vendas 2 = 3
vendas 3 = 9
vendas 4 = 12

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas (n-1) + vendas n

maxi m n
 | m>=n = m
 | otherwise = n

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = maxi (maxVendas(n-1)) (vendas n)

mOr :: Bool -> Bool -> Bool
mOr True True = True
mOr True False = True
mOr False True = True
mOr False False = False

mOr2 :: Bool -> Bool -> Bool
mOr2 True x = True
mor2 False x = x

mOr3 :: Bool -> Bool -> Bool
mOr3 True _ = True -- Ele não vai nem avaliar o segundo argumento
mor3 False x = x

{- Uma definição diferente para fatorial que previni o estouro da pilha -}
tailFat :: Integer -> Integer -> Integer
tailFat 0 x = x 
tailFat n x = tailFat (n-1) (n*x) -- x é o valor parcial do fatorial

fat n = tailFat n 1

{-
 tailFat 4 1
 = tailFat 3 4
 = tailFat 2 12
 = tailFat 1 24
 = tailFat 0 24
-}

{- Definir operadores -}
(&&&) :: Integer -> Integer -> Integer
x &&& y 
 | x > y     = y
 | otherwise = x
infixl 7 &&&

{----------------------------------------------------------------}
{- Tuplas em Haskell 
 (t1, t2, ..., tn)
 (v1 :: t1, v2 :: t2, ..., vn :: tn)
 Os tipos podem ser diferentes entre si
 Ex: (Int, Bool, Char)
     (123, True, `ab`)
-}

minMax :: Integer -> Integer -> (Integer, Integer)
minMax x y
 | x > y     = (y, x)
 | otherwise = (y, x)

addPair :: (Int, Int) -> Int
addPair (x, y) = x + y
-- Chamada: addPair (2, 3)

addPair0 :: (Int, Int) -> Int
addPair0 (0, y) = y
addPair0 (x, 0) = x
addPair0 (x, y) = x + y
{----------------------------------------------------------------}
