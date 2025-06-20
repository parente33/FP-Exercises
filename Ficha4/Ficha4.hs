module Ficha4 where
import Data.Char (isDigit, isAlpha)

-- Ex1
digitAlpha :: String -> (String, String)
digitAlpha = foldr (\c (as, ds) -> if isAlpha c then (c:as, ds)
                                   else if isDigit c then (as, c:ds)
                                   else (as, ds)) ("", "")

-- Ex2
nzp :: [Int] -> (Int, Int, Int)
nzp = foldr f (0, 0, 0)
  where
    f x (n, z, p)
      | x < 0     = (n+1, z, p)
      | x == 0    = (n, z+1, p)
      | otherwise = (n, z, p+1)

-- Ex3
divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = go x 0
  where
    go a q
      | a < y     = (q, a)
      | otherwise = go (a - y) (q + 1)

-- Ex4
fromDigits :: [Int] -> Int
fromDigits = go 0
  where
    go acc [] = acc
    go acc (d:ds) = go (acc * 10 + d) ds

-- Ex5
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit = go 0 0
  where
    go _   maxSoFar []     = maxSoFar
    go acc maxSoFar (x:xs) = let acc' = acc + x
                             in go acc' (max acc' maxSoFar) xs

-- Ex6
fib :: Int -> Int
fib n = fibAux n 0 1
  where
    fibAux 0 a _ = a
    fibAux n a b = fibAux (n - 1) b (a + b)

-- Ex7
intToStr :: Integer -> String
intToStr n
  | n < 0     = '-' : intToStr (-n)
  | n == 0    = "0"
  | otherwise = reverse (go n "")
  where
    go 0 acc = acc
    go x acc = let (q, r) = divMod x 10
               in go q (toEnum (fromEnum '0' + fromIntegral r) : acc)

-- Ex8

-- a)
{-
listaHa = [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
-- Alternativa: múltiplos de 6
listaHaAlt = [6,12,18]
-}

-- b)
{-
listaHb = [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
-- É igual à (a)
listaHbAlt = [6,12,18]
-}

-- c)
{-
listaHc = [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
-- Alternativa:
listaHcAlt = [(x,30-x) | x <- [0..30], x <= 20, 30-x <= 20]
-}

-- d)
{-
listaHd = [sum [y | y <- [1..x], odd y] | x <- [1..10]]
-- Alternativa:
listaHdAlt = scanl1 (+) [1,1..] -- Soma dos ímpares até x para cada x (mais complexa)
-}

-- Ex9

-- a)
-- listaIa = [2^x | x <- [0..10]]

-- b)
-- listaIb = [(x,6-x) | x <- [1..5]]

-- c)
-- listaIc = [ [1..x] | x <- [1..5] ]

-- d)
-- listaId = [ replicate x 1 | x <- [1..5] ]

-- e)
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- listaIe = [fatorial x | x <- [1..6]]