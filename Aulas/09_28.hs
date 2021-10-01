--offset :: Int
--offset = fromEnum `A` - fromEnum `a`

--paraMaiusculo :: Char -> Char
--paraMaiusculo ch = toEnum (ch + offset)

{-
 Concatenação -> ++
 Concatenação é uma função polimorfica
-}

meuNome :: String
meuNome = "Jose" ++ " " ++ "Henrique"

{-
 Números pontos flutuantes
 Notação cientifica = e-4 ou e+4
 para arredondar para baixo = floor
 para arredondar para cima = ceiling
 para arredondaer um ponto flutuante = round
-}

{----------------------------------------------------------------}
{------------------- Definição de Funções -----------------------}
{----------------------------------------------------------------}
todosIguaisInt :: Int -> Int -> Int -> Bool
todosIguaisInt m n p = (m == n) && (n == p)

todosIguaisBool :: Bool -> Bool -> Bool -> Bool
todosIguaisBool m n p = (m == n) && (n == p)

todosIguais m n p = (m == n) && (n == p)

{-
 Definição Local
 where (onde)
-}
somaQuadrados :: Int -> Int -> Int
somaQuadrados x y = quadrado x + quadrado y
 where
     quadrado :: Int -> Int
     quadrado x = x * x

-- expressao let
-- let <associacoes> in <expressao>
-- let x = 2 in x + 5
-- let x = 2 ; y = 3 in x + y

somaQuadradosLet :: Int -> Int -> Int
somaQuadradosLet x y = let quadradoX = x * x
                           quadradoY = y * y
                        in
                           quadradoX + quadradoY

{-
 Recursividade
 caso base -> parada
 caso recursivo -> chamada recursiva

 funcRec :: tipo
 funcRec param = caso base
 funcRec param = caso recursivo
-}

fatorial :: Integer -> Integer -- Guarda ou Recursão primitiva
fatorial n
 | n == 0 = 1                     -- fat.1
 | otherwise = fatorial(n-1) * n  -- fat.2

{-
 (fatorial 3)             [fat.2]
 (fatorial 2) * 3         [fat.2]
 (fatorial 1) * 2 * 3     [fat.2]
 (fatorial 0) * 1 * 2 * 3 [fat.1]
 1 * 1 * 2 * 3            [pela aritmetica]
 6                        [pela aritmetica]
-}

fatorialCP :: Integer -> Integer   -- Casamento de Padrão
fatorialCP 0 = 1                   -- caso base
fatorialCP n = fatorial (n-1) * n  -- caso recursivo

fib :: Integer -> Integer
fib n
 | n == 0 = 0
 | n == 1 = 1
 | n > 1 = fib (n-2) + fib (n-1)