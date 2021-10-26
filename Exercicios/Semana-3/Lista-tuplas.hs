{- Questão 01 -}
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (menor, maior)
 where
     menor 
         | x < y && x < z = x
         | y < x && y < z = y
         | otherwise      = z
     maior
         | x > y && x > z = x
         | y > x && y > z = y
         | otherwise      = z
{--------------}

{- Questão 02 -}
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z)
 | x > y     = ordenaTripla (y, x, z)
 | y > z     = ordenaTripla (x, z, y)
 | otherwise = (x, y, z)
{--------------}

{- Questão 03 -}
type Ponto = (Float, Float)
type Reta  = (Ponto, Ponto)

primeiraCoordenada :: Ponto -> Float
primeiraCoordenada (x, _) = x

segundaCoordenada :: Ponto -> Float
segundaCoordenada (_, y) = y

ehVertical :: Reta -> Bool
ehVertical ((x1, _), (x2, _))
 | x1 == x2  = True
 | otherwise = False
{--------------}

{- Questão 04 -}
pontoY :: Float -> Reta -> Float
pontoY x ((x1, y1), (x2, y2)) = (y2-y1) / (x2-x1) * (x-x1) + y1
-- pontoY 5 ((1, 2), (3, 4))
{--------------}
