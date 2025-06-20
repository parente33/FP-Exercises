module Ficha5 where
import Data.Ord (comparing)
import Data.List (groupBy, sortBy,transpose)
import Data.Function (on)

-- Ex1

-- a)
any' :: (a -> Bool) -> [a] -> Bool
any' _ []     = False
any' p (x:xs) = p x || any' p xs

-- b)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = x:xs

-- e)
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' p (x:xs)
  | p x       = let (yes, no) = span' p xs in (x:yes, no)
  | otherwise = ([], x:xs)

-- f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' eq a (x:xs)
  | eq a x    = xs
  | otherwise = x : deleteBy' eq a xs

-- g)
sortOn1 :: Ord b => (a -> b) -> [a] -> [a]
sortOn1 f [] = []
sortOn1 f (h:t) = insert f h (sortOn1 f t) 

insert :: Ord b => (a -> b) -> a -> [a] -> [a]
insert f x [] = [x]
insert f x (h:t)
    | f x <= f h = x:h:t
    | otherwise = h:insert f x t

sortOnfldr :: Ord b => (a -> b) -> [a] -> [a]
sortOnfldr f = foldr (insert f) []


-- Ex2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--selgrau :: Int -> Polinomio -> Polinomio
--selgrau n pol = filter (\(c,e)->e==n) pol

--selgrau' :: Int -> Polinomio -> Polinomio
--selgrau' n pol = filter (\x-> (snd x)==n) pol

-- a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n pol = filter ((==n).snd) pol

-- b)
conta :: Int -> Polinomio -> Int
conta n ((c,e):t) = if e==n
                        then 1 + conta n t
                        else conta n t

{-conta1 :: Int -> Polinomio -> Int
  conta1 n pol = foldr aux 0 pol
    where aux (c,e) r = if e==n
                          then 1+r
                          else r
-}

-- c)
grau :: Polinomio -> Int
grau p = maximum (map snd p)

-- d)
deriv :: Polinomio -> Polinomio
deriv ((a,b):t) = (a*(fromIntegral b),b-1) : deriv t
deriv [] = []

--deriv' :: Polinomio -> Polinomio
--deriv' pol = map (\(c,e)->(c*(fromIntegral e),e-1)) pol

--deriv2 :: Polinomio -> Polinomio
--deriv2 pol = foldr (\(c,e) r -> (c*(fromIntegral e), e-1):r) [] pol

-- e)
calcula :: Float -> Polinomio -> Float
calcula x ((c,e):t) = c*x^e + calcula x t
calcula x [] = 0

--calcula1 :: FLoat -> Polinomio -> FLoat
--calcula1 x pol = foldr (\(c,e) r -> c*x^e + r) 0 pol

--calcula2 :: Float -> Polinomio -> Float
--calcula2 x pol = sum (map (\c,e)-> c*x^e) pol)

-- f)
simp :: Polinomio -> Polinomio
simp = filter (\(c, _) -> c /= 0)

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult (a, n) = map (\(b, m) -> (a * b, n + m))

-- h)
ordena :: Polinomio -> Polinomio
ordena = sortBy (comparing snd)

-- i)
normaliza :: Polinomio -> Polinomio
normaliza = map somaGrupo . groupBy ((==) `on` snd) . ordena
  where
    somaGrupo ms@((_, g):_) = (sum (map fst ms), g)
    somaGrupo [] = (0, 0)  -- caso técnico, nunca ocorre se a entrada não for vazia

-- j)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

-- k)
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza (concatMap (`mult` p2) p1)

-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = simp (normaliza p1) == simp (normaliza p2)


-- Ex3

type Mat a = [[a]]

-- a)
dimOK :: Mat a -> Bool
dimOK [] = True
dimOK (l:ls) = all (\linha -> length linha == length l) ls

-- b)
dimMat :: Mat a -> (Int, Int)
dimMat [] = (0, 0)
dimMat m@(l:_) = (length m, length l)

-- c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWMat (+)

-- d)
transpose' :: Mat a -> Mat a
transpose' ([]:_) = []
transpose' m = map head m : transpose' (map tail m)

-- e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat a b = [[sum $ zipWith (*) linha col | col <- transpose b] | linha <- a]

-- f)
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f = zipWith (zipWith f)

-- g)
triSup :: (Num a, Eq a) => Mat a -> Bool
triSup m = all (\(i, linha) -> all (== 0) (take i linha)) (zip [0..] m)

-- h)
rotateLeft :: Mat a -> Mat a
rotateLeft = reverse . transpose