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
