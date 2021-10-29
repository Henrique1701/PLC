{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- Questão 01 -}
import System.Directory.Internal.Prelude (toUpper)
paraMaiuscula :: String -> String
paraMaiuscula l = [toUpper x | x <- l, x `elem` ['A'..'Z'] || x `elem` ['a'..'z']]
{--------------}

{- Questão 02 -}
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n = divisores n == [1, n]
{--------------}

{- Questão 03 -}
menorLista :: [Integer] -> Integer
menorLista (x:[]) = x
menorLista (x:y:xs)
 | x > y = menorLista (y:xs)
 | otherwise = menorLista (x:xs)
{--------------}

{- Questão 04 -}
fibTable :: Integer -> String 
fibTable n = cabecalho ++ "\n" ++ fibCorpo n 0

cabecalho :: String
cabecalho = "n " ++ " fib n"

fibCorpo :: Integer -> Integer -> String
fibCorpo n indece
 | n == indece = show (indece) ++ "  " ++ show (fib indece) ++ "\n"
 | otherwise = show (indece) ++ "  " ++ show (fib indece) ++ "\n" ++ fibCorpo (n) (indece+1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = tailFib n 0 1 2

tailFib :: Integer -> Integer -> Integer -> Integer -> Integer 
tailFib parada anterior2 anterior1 indece
 | indece == parada = anterior1 + anterior2
 | otherwise = tailFib parada anterior1 (anterior2 + anterior1) (indece + 1)

-- Como testar: putStr (fibTable 8)
{--------------}

{- Questão 05 -}
measure :: [a] -> Integer
measure [] = -1
measure (x:[]) = 1
measure (x:xs) = 1 + measure xs
{--------------}

{- Questão 06 -}
takeFinal :: Integer -> [a] -> [a]
takeFinal _ [] = []
takeFinal n (x:xs) 
 | n >= measure (x:xs) = (x: takeFinal (n-1) xs)
 | otherwise = takeFinal n xs
{--------------}

{- Questão 07 -}
remove :: Integer -> [a] -> [a]
remove _ [] = []
remove n (x:xs)
 | n == 0 = xs
 | otherwise = x : remove (n-1) xs
{--------------}

{- Questão 08 -}
retornaPrimeiroInteiro :: [Integer] -> Integer
retornaPrimeiroInteiro [] = 0
retornaPrimeiroInteiro (x:_) = x

retornaPrimeiroInteiro2 :: [Integer] -> Integer
retornaPrimeiroInteiro2 l 
 | measure l > 0 = head l
 | otherwise = 0
{--------------}

{- Questão 09 -}
somaDoisPrimeiros :: [Integer] -> Integer
somaDoisPrimeiros [] = 0
somaDoisPrimeiros (x:[]) = x
somaDoisPrimeiros (x:y:_) = x + y

somaDoisPrimeiros2 :: [Integer] -> Integer
somaDoisPrimeiros2 l 
 | measure l >= 2 = head l + head (tail l)
 | measure l == 1 = head l
 | otherwise = 0
{--------------}

{- Questão 10 -}
produto :: [Integer] -> Integer
produto [] = 1
produto (x:[]) = x
produto (x:xs) = x * produto xs
{--------------}

{- Questão 11 -}
unique :: [Integer] -> [Integer]
unique [] = []
unique (x:xs) 
 | notElem x xs = x : unique xs
 | otherwise = unique (removeTodos x xs)

removeTodos :: Integer -> [Integer] -> [Integer]
removeTodos _ [] = []
removeTodos n (x:xs)
 | n == x = removeTodos n xs
 | otherwise = x : removeTodos n xs
{--------------}

{- Questão 12 -}
emOrdemCrescente :: [Integer] -> Bool
emOrdemCrescente [] = True
emOrdemCrescente (x:[]) = True
emOrdemCrescente (x:y:xs)
 | x > y = False
 | otherwise = emOrdemCrescente (y:xs)
{--------------}