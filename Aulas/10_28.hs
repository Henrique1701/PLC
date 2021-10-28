{-
let dobroLista = map (*2)
dobroLista [1,2,3,4,5] = [2,4,6,8,10]

((>5) . (foldr (+) 0) . map (*3))
 :: (Ord a, Num a) => [a] -> Bool

(.) :: (b->c) -> (a->b) -> a -> c
-}

{- O que a gente já viu:
Tipos básicos (Int, Float, Char, Bool ...)
Tipos compostos () - Tuplas. [] - Listas
Sinônimos de tipo: type
Funções/Expressões
 casam padrão
 guardas
 expr case
 funções como valores
 composição de funções
-}

{- Tipos algébricos
- palavra resevada: data
    data NomeTipo = valores do tipo
- enumereção
-}

data Temperatura = Quente | Fria
                     deriving (Show, Eq, Ord, Enum, Read)
data Estacao     = Verao | Outono | Inverno | Primavera
                     deriving (Show, Eq, Ord, Enum, Read)
-- Verao == Verao
-- Verao /= Outono
-- Vera < Outono
-- Verao <= Verao
-- [Verao .. Primavera]
-- read "Verao" :: Estacao

clima :: Estacao -> Temperatura
clima Inverno = Fria
clima _     = Quente
