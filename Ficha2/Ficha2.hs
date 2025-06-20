module Ficha2 where
import Data.Char
import Data.List (sortBy)
import Data.Ord (comparing)

-- Ex 2

-- a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2*x : dobros xs

-- b)
numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (x:xs) = if c == x
                        then 1 + numOcorre c xs
                        else numOcorre c xs

-- c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) = not (x < 0) && positivos xs

-- d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if x < 0
                then soPos xs
                else x : soPos xs

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) = if x < 0 then x + somaNeg xs
                  else somaNeg xs

-- f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) = if length xs <= 2 then (x:xs)
                    else reverse (take 3 (reverse xs))

tresUlt' :: [a] -> [a]
tresUlt' (a:b:c:d:xs) = tresUlt (b:c:d:xs)
tresUlt' l = l

-- g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):xs) = b : segundos xs

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((x,y):xs) = (a == x) || nosPrimeiros a xs

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):xs) =
    let (d,e,f) = sumTriplos xs
    in  (a + d,b + e,c + f)


-- Ex3

-- a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs)
    | ord x >= 48 && ord x <= 57 = x : soDigitos xs
    | otherwise = soDigitos xs

-- b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs)
    | ord x >= 97 && ord x <= 122 = 1 + minusculas xs
    | otherwise = minusculas xs

-- c)
{-
nums :: String -> [Int]
nums [] = []
nums (x:xs) 
    | ord x >= 48 && ord x <= 57 = chr x : chr (nums xs)
    | otherwise = nums xs
-}

--
nums :: String -> [Int]
nums [] = []
nums (x:xs)
    | isDigit x = digitToInt x : nums xs
    | otherwise = nums xs


-- Ex4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)
conta :: Int -> Polinomio -> Int
conta n p = length [ (c, e) | (c, e) <- p, e == n ]

-- b)
grau :: Polinomio -> Int
grau p = maximum [ e | (_, e) <- p ]

-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = [ (c, e) | (c, e) <- p, e == n ]

-- d)
deriv :: Polinomio -> Polinomio
deriv p = [ (c * fromIntegral e, e - 1) | (c, e) <- p, e > 0 ]

-- e)
calcula :: Float -> Polinomio -> Float
calcula x p = sum [ c * (x ^^ e) | (c, e) <- p ]

-- f)
simp :: Polinomio -> Polinomio
simp p = [ (c, e) | (c, e) <- p, c /= 0 ]

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult (c1, e1) p = [ (c1 * c2, e1 + e2) | (c2, e2) <- p ]

-- h)
normaliza :: Polinomio -> Polinomio
normaliza p = [ (sum [c | (c, e') <- p, e' == e], e) | e <- graussUnicos ]
  where graussUnicos = removeDuplicatas [ e | (_, e) <- p ]

removeDuplicatas :: Eq a => [a] -> [a]
removeDuplicatas [] = []
removeDuplicatas (x:xs) = x : removeDuplicatas (filter (/= x) xs)

-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

-- j)
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza [ (c1 * c2, e1 + e2) | (c1, e1) <- p1, (c2, e2) <- p2 ]

-- k)
ordena :: Polinomio -> Polinomio
ordena = sortBy (comparing snd)

-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (simp (normaliza p1)) == ordena (simp (normaliza p2))