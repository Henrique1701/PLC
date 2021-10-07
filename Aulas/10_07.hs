{-      LISTAS
 - length -> retorna o tamanho da lista
  - Ex: length [1 .. 10] == 10
 - Função (:) constroí listas
  - Ex: (:) 1 [2, 3] == [1, 2, 3]
 - Função (++) concatena listas
  - Ex: (++) [1, 2] [3, 4] == [1, 2, 3, 4]
 - Função (!!) indexa 
  - Ex: (!!) [3, 4, 5] 1 == 4
 - Função splitAt divide listas
  - Ex: splitAt 2 [1, 2, 3, 4] = ([1, 2], [3, 4])
 - Função drop descarta os primeiros valores
  - Ex: drop 3 [1, 2, 3, 4] == [4]
 - Função take pegar os primeiros valores
  - Ex: take 3 [1, 2, 3, 4] == [1, 2, 3]
 - init [1 .. 10]
 - Função reverse reverte uma lista
  - Ex: reverse [1, 2, 3] = [3, 2, 1]
 - Função replicate  
  - Ex: replicate 5 'a' = "aaaaa"
-}

tamanhoListaInt :: [Int] -> Int
tamanhoListaInt []                         = 0
tamanhoListaInt (cabecaLista : caldaLista) = 1 + tamanhoListaInt caldaLista

somaValoresLista :: [Int] -> Int
somaValoresLista [] = 0
somaValoresLista (head : tail) = head + somaValoresLista tail

tamLista :: [t] -> Int -- Polimorfismo paramétrico
tamLista [] = 0
tamLista (x : xs) = 1 + tamLista xs

mhead :: [t] -> t
mhead [] = error "Lista vazia"
mhead (x : _ ) = x

segundoElemento :: [t] -> t
segundoElemento (_ : x : _) = x

maiorValorLista :: Ord t => [t] -> t
maiorValorLista [x] = x -- Lista só tem um elemento
maiorValorLista (x:xs)
 | x > maiorValorLista xs = x
 | otherwise              = maiorValorLista xs
