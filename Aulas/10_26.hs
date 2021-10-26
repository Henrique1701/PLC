{- Funções parciais -}

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = (x == y) && (y == z)

allEqual1 :: Int -> Int -> Bool
allEqual1 = allEqual 1

allEqual2 :: Int -> Bool
allEqual2 = allEqual1 2

allEqual3 :: Bool
allEqual3 = allEqual2 3
-- allEqual3 == False
-- allEqual3 == allEqual 1 3 3
-- Os elementos são consumidos da esquerda para direita

{- Associatividade da aplicação de função (esquerda)
f :: T1 -> T2 -> T3 -> T4
f e1 e2 e3
f 5 = (f 5) e2 e3
-}

{- Associatividade da seta (direita)
f :: T1 -> T2 -> T3 -> T4
f :: T1 -> (T2 -> (T3 -> T4))
f e1 :: T2 -> (T3 -> T4)
(f e1) e2 :: T3 -> T4
-}

{- Operador ($)
Utilizado para compor funções de forma associativa a direita
Ex.: not(ehPar(inc 3)) == not $ ehPar & inc 3
-}

mzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
mzipWith f (a:as) (b:bs) = f a b : mzipWith f as bs
mzipWith f _      _      = [] -- Uma das duas listas está vazia
-- mzipWith ($) [sum, product] [[1..6], [5..9]]
{-
mzipWith ($) [sum, product] [[1..6], [5..9]]
= ($) sum [1..6] : mzipWith ($) [product] [[5..9]]
= ($) sum [1..6] : (($) product [5..9]) : mzipWith ($) [] []
= ($) sum [1..6] : (($) product [5..9]) : []
= 21 : 15120 : []
= [21, 15120]
-}

{- Notação Lambda
Para definir funções anônimas:
(\ x -> x + 1) 3 = 4
(\ x y -> x + y + 1) 3 5 = 9
map (\x -> x*20) [1..5]
(filter (\x -> x > 70) . map (\x -> x*20)) [1..5]
-}

f1 l = (filter (\x -> x > 70) . map (\x -> x*20)) l

addNum :: Int -> (Int -> Int)
addNum x = h
 where
     h y = x + y

addNumLambda :: Int -> (Int -> Int)
addNumLambda x = (\y -> x + y)

multiplica :: Int -> Int -> Int
multiplica x y = x * y

mDivFloat :: Float -> Float -> Float
mDivFloat x y = x / y

{-
multiplica 2
= (\y -> 2 * y)

mDivFloat 2
= (\y -> 2 / y)
-}

mListaDobro :: [Int] -> [Int]
mListaDobro l = map (multiplica 2) l