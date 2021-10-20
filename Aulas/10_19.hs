{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- Polimorfismo -}
{- 
    Função é um valor em Haskel
    Funções de alta ordem
        Uma função de alta ordem recebe uma função como parametro
-}
total :: (Int -> Int) -> Int -> Int 
total f 0 = f 0
total f n = f n + total f (-1)

vendas :: Int -> Int
vendas 0 = 3
vendas 1 = 7
vendas 2 = 10

{-
total vendas 2
= vendas 2 + total vendas 1
= vendas 2 + vendas 1 + total vendas 0
= vendas 2 + vendas 1 + vendas 0
= 10 + 7 + 3
= 20
-}

maxi :: Int -> Int -> Int 
maxi m n
 | m >= n     = m
 | otherwise  = n

maxFun :: (Int -> Int) -> Int -> Int 
maxFun f 0 = f 0
maxFun f n = maxi (maxFun f (n-1)) (f n)

{-
maxFun vendas 2
= maxi (maxFun vendas 1) (vendas 2)
= maxi (maxi (maxFun vendas 0) (vendas 1)) (vendas 2)
= maxi (maxi (vendas 0) (vendas 1)) (vendas 2)
= maxi (maxi (vendas 0) (vendas 1)) (vendas 2)
= maxi (maxi 4 7) (vendas 2)
= maxi 7 (vendas 2)
= maxi 7 10
= 10
-}

{-
    Mapeamento
-}

dobraValoresLista :: [Int] -> [Int]
dobraValoresLista []     = []
dobraValoresLista (x:xs) = (2 * x) : dobraValoresLista xs

quadValoresLista :: [Int] -> [Int]
quadValoresLista []     = []
quadValoresLista (x:xs) = (x * x) : quadValoresLista xs

operValoresLista :: (Int -> Int) -> [Int] -> [Int]
operValoresLista f []     = []
operValoresLista f (x:xs) = (f x) : operValoresLista f xs

quad :: Int -> Int
quad x = x * x

map1 :: (t -> t) -> [t] -> [t]
map1 f []     = []
map1 f (x:xs) = (f x) : map1 f xs
-- map1 not [True, False]

map2 :: (t -> u) -> [t] -> [u]
map2 f []     = []
map2 f (x:xs) = (f x) : map2 f xs
-- map2 ehPar [1, 2, 3, 4]
-- map2 snd [(1, 'a'), (2, 'b'), (3, 'c')]
-- map2 length [[1..2], [3..9], [2..7]]

ehPar :: Int -> Bool 
ehPar x = x `mod` 2 == 0

-- Mapeamento com compreensão de lista:
-- [length x | x <- [[1..2], [3..9], [2..7]]]

{-
Filtro
-}

filtrarPares :: [Int] -> [Int]
filtrarPares []     = []
filtrarPares (x:xs)
 | ehPar x = x : filtrarPares xs
 | otherwise = filtrarPares xs
-- filtrarPares [1..8]

filtrarMaiorQue5 :: [Int] -> [Int]
filtrarMaiorQue5 []     = []
filtrarMaiorQue5 (x:xs)
 | maiorQue5 x = x : filtrarMaiorQue5 xs
 | otherwise = filtrarMaiorQue5 xs
-- filtrarMaiorQue5 [1..10]

filtro :: (t -> Bool) -> [t] -> [t]
filtro f []     = []
filtro f (x:xs)
 | f x = x : filtro f xs
 | otherwise = filtro f xs
-- filtro maiorQue5 [1..10]
-- filtro ehPar [1..10]

maiorQue5 :: Int -> Bool 
maiorQue5 x = x > 5

somaLista :: [Int] -> Int
somaLista [] = 0 -- 0 é o elemento neutro
somaLista (x:xs) = (+) x (somaLista xs)
-- somaLista [1..5]

prodLista :: [Int] -> Int 
prodLista [] = 1 -- 1 é o elemento neutro
prodLista (x:xs) = (*) x (prodLista xs)
-- prodLista [1..5]

andLista :: [Bool] -> Bool
andLista [] = True -- True é o elemento neutro
andLista (x:xs) = (&&) x (andLista xs)

{-
andLista [True]
= (&&) True (andLista [])
= (&&) True True
= True
-}

{-
andLista [True]
= (&&) True (andLista [])
= (&&) True True
= True
-}

-- No GHCI, tem o foldr
mfoldr :: (t -> t -> t) -> t -> [t] -> t
mfoldr f v []     = v
mfoldr f v (x:xs) = f x (mfoldr f v xs)
-- mfoldr (+) 0 [1..6]

{-
mfoldr o 'r' vem de right.
    Pois a opereção é aplicada da direira para esquerda

mfoldr (+) 0 [1, 2, 3]
= (+) 1 (mfoldr (+) 0 [2, 3])
= (+) 1 ((+) 2 (mfoldr (+) 0 [3]))
= (+) 1 ((+) 2 ((+) 3 (mfoldr (+) 0 [])))
= (+) 1 ((+) 2 ((+) 3 0))
= (+) 1 ((+) 2 3)
= (+) 1 5
= 6
-}