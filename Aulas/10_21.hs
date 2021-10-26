{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- Folding -}
mfoldr1 :: (a -> a -> a) -> [a] -> a
mfoldr1 f [v]    = v
mfoldr1 f (x:xs) = f x (mfoldr1 f xs)

mfoldl :: (b -> a -> b) -> b -> [a] -> b
mfoldl f v []     = v
mfoldl f v (x:xs) = mfoldl f (f v x) xs
-- mfoldl (+) 0 [1, 2, 3]

{-
mfoldl (+) 0 [1, 2, 3]
= mfoldl (+) ((+) 0 1) [2, 3]
= mfoldl (+) 1 [2, 3]
= mfoldl (+) ((+) 1 2) [3]
= mfoldl (+) 3 [3]
= mfoldl (+) ((+) 3 3) []
= mfoldl (+) 6 [0]
= 6
-}

mOr :: [Bool] -> Bool 
mOr l = mfoldl (||) False l
-- mOr [True, False, False]
-- mOr [False, False, False]

paraUm :: t -> Int 
paraUm x = 1

comprimentoLista :: [t] -> Int
comprimentoLista l = foldl1 (+) (map paraUm l)
-- comprimentoLista [True, False, False, True]

{-
    Composição de função

(.) :: (b -> c) -> (a -> b) -> a -> c
Obs: Avaliar da direita para esquerda

(f . g) x = f(g(x))

(not . not) True = True
-}

ehPar :: Int -> Bool 
ehPar x = x `mod` 2 == 0
-- (not . ehPar) 3

quad :: Int -> Int
quad x = x * x
-- (not . ehPar) ((quad . quad) 3)

twice :: (t -> t) -> t -> t
twice f x = f (f x)
-- twice not True
{-
twice not True
= not (not True)
= not False
= True
-}

twice2 :: (t -> t) -> t -> t
twice2 f x = (f . f) x
-- twice2 not True

inc :: Int -> Int
inc x = x + 1

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = (f . f)
iter n f = (iter (n-1) f) . f

{-
iter 3 g
= (iter 2 g) . g
= ((iter 1 g) . g) . g
= (((iter 0 g) . g) . g) . g
= ((id . g) . g) . g
-}
