{- Tuplas aninhamento-}
shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (a, (b, c))

{- Biblioteca -}
primeiro :: (Int, Int) -> Int
--primeiro (a, b) = a
-- existe uma função para isso fst (para segundo snd)
primeiro (a, _) = a

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-2) + fib(n-1)

fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

fibPair :: Integer -> (Integer, Integer)
fibPair n
 | n == 0 = (0, 1)
 | otherwise = fibStep (fibPair(n-1))

fastFib :: Integer -> Integer
fastFib n = fst ( fibPair(n) )

{- No slide sobre tuplas
   tem a definição de uma função que retorna as raizes da equação do segundo grau
-}

{- Sinonimos de tipos -}
type Nome     = String
type Idade    = Int
type Telefone = String
type Pessoa   = (Nome, Idade, Telefone)

nome :: Pessoa -> (Nome)
nome (n, i, t) = "O nome da pessoa eh: " ++ n
-- Ex: nome ("Henrique", 22, "40028922")

nomeIdade :: Pessoa -> String
nomeIdade (n, i, _) = n ++ " tem " ++ show(i) ++ " anos"
-- Ex: nomeIdade ("Henrique", 22, "40028922")

{----------------}
{-    Listas    
 - Listas precisam ser de tipois homogeneos
  - Ex: [1, 2, 3], [True, True, False], ["oi", "tudo", "bem"]

 - Podemos ter listas de tuplas ou de listas
  - Ex: [(1, True), (2, False)], [[1, 2, 3], [4, 5]]

 - Lista vazia, pode ser de qualquer tipo: []

 - Listas podem ser comparadas por igualdade
  - Ex: [] == [] (True); [1] == [1, 1] (False); [1, 2] == [2, 1] (False)

 - Forma simplificada de preencher listas usando ".."
  - Ex: [1 .. 10]; [-2 .. 5]; [-7 .. (-2)]; ['a' .. 'z']; [2.4 .. 9.2]
  - Usando passo: [1, 4 .. 10]; [10, 8 .. 0]; [1.3, 1.7 .. 3.2]

-     Compreensão de listas
 - [ exp | gerador]
 - [ exp | gerador, expBool]
  - Ex: {x > 0 | x e N}
  -     [x | x <- [1 .. 10]]
  -     [x+1 | x <- [1 .. 10]]
  -     [x^2 | x <- [1 .. 100]]
  -     [1 | x <- [1 .. 10]]
-}

dobraLista :: [Int] -> [Int]
dobraLista l = [ x*2 | x <- l]

ehPar :: Int -> Bool
ehPar x = x `mod` 2 == 0

dobraListaPar :: [Int] -> [Int]
dobraListaPar l = [2 * x | x <- l, ehPar x]

triangulos = [(a, b, c) | c <- [1 .. 9], b <- [1 .. 9], a <- [1 .. 9],
              a < b + c, b < a + c, c < a + b]

triangulosRetangulos = [(a, b, c) | c <- [1 .. 9], b <- [1 .. 9], a <- [1 .. 9], 
                        a ^ 2 + b ^ 2 == c ^ 2]

triangulosRetangulosPerim24 = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10], 
                        a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]
{----------------}

