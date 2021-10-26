{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{---- Listas ----}
mzip :: [a] -> [b] -> [(a, b)]
mzip (x:xs) (y:ys) = (x, y) : mzip xs ys
mzip []     (y:ys) = []
mzip (x:xs) []     = []
mzip []     []     = []

mzip2 :: [a] -> [b] -> [(a, b)]
mzip2 (x:xs) (y:ys) = (x, y) : mzip2 xs ys
mzip2 []     _      = []
mzip2 _      []     = []

mzip3 :: [a] -> [b] -> [(a, b)]
mzip3 (x:xs) (y:ys) = (x, y) : mzip3 xs ys
mzip3 _      _      = []

mtake :: Int -> [a] -> [a]
mtake 0 _      = []
mtake n []     = []
mtake n (x:xs) = x : mtake (n-1) xs

mdrop :: Int -> [a] -> [a]
mdrop 0 l      = l
mdrop _ []     = []
mdrop n (x:xs) = mdrop (n-1) xs

-- Ordenação por insercao
iSort :: [Int] -> [Int]
iSort []     = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x []     = [x]
ins x (y:ys)
 | x <= y    = x : (y:ys)
 | otherwise = y : ins x ys

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = menorIgualX ++ [x] ++ maiorX
 where
     menorIgualX = qSort [y | y <- xs, y <= x]
     maiorX = qSort [y | y <- xs, y > x]


{-  Polimorfismo -}
(+++) :: [t] -> [t] -> [t]
[]     +++ y = y
(x:xs) +++ y = x : (xs +++ y)

rev :: [t] -> [t]
rev []     = []
rev (x:xs) = rev xs ++ [x]

exExpCase1 :: String
exExpCase1 = case True of
                    True -> "verdadeiro"
                    False -> "falso"

exExpCase2 :: Bool -> String 
exExpCase2 b = "Por chamar a funcao exExpCase2 " ++
               case b of
                 True -> "verdadeiro"
                 False -> "false"
                 ++ " eh o resultado"

head1 :: [a] -> a
head1 []     = error "Nao hah valor em uma lista vazia"
head1 (x:xs) = x

head2 :: (Show a) => [a] -> String
head2 l = "Uma lista " ++
          case l of
            [] -> "vazia"
            (x:_) -> "com valor " ++ show x ++ " na cabeca"
            