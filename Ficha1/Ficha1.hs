module Ficha1 where
import Data.Char (ord, chr)

-- Ex1

-- a)
perimetro :: Float -> Float
perimetro lado = 2 * pi * lado

-- b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (a,b) (c,d) = sqrt ((a - c) ^ 2 + (b - d) ^ 2)

-- c)
primUlt :: [a] -> (a,a)
primUlt lista = (head lista,last lista)

-- d)
multiplo :: Integer -> Integer -> Bool
multiplo m n = mod m n == 0

multiplo' :: Integer -> Integer -> Bool
multiplo' m n = mod m n == 0

-- e)
truncaImpar :: [a] -> [a]
truncaImpar lista = if even (length lista)
                        then lista
                        else tail lista

truncaImpar' :: [a] -> [a]
truncaImpar' lista = if even (length lista)
                        then lista
                        else tail lista

-- f)
max2 :: Integer -> Integer -> Integer
max2 a b = max a b

max2' :: Integer -> Integer -> Integer
max2' = max

-- g)
max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = max2 (max2 a b) c


-- Ex2

-- a)
nRaizes :: Int -> Int -> Int -> Int
nRaizes a b c
  | b ^ 2 - 4 * a * c == 0 = 1
  | b ^ 2 - 4 * a * c > 0 = 2
  | otherwise = 0

nRaizes' :: Float -> Float -> Float -> Int
nRaizes' a b c
  | delta < 0 = 0
  | delta == 0 = 1
  | otherwise = 2
  where
      delta = b ^ 2 - 4 * a * c


-- b)
raizes :: Float -> Float -> Float -> (Float,Float)
raizes a b c = (x,y)
        where x = (-b + sqrt(b ^ 2 - 4 * a * c)) / 2 * a
              y = (-b - sqrt(b ^ 2 - 4 * a * c)) / 2 * a

{-raizes' :: Float -> Float -> Float -> (Float,Float)
raizes' a b c = if n==0 then []
                else if n==1 then-b/(2*a)
                     else [-b + sqrt d]
    where n = nRaizes a b c
          d = b^2 - 4*a*c
-}

-- Ex4

data Hora = H Int Int deriving (Show,Eq)

-- a)
validaHoras4 :: Hora -> Bool
validaHoras4 (H h m) = 0 <= h && h < 24 && 0 <= m && m < 60

-- b)
depoisHoras4 :: Hora -> Hora -> Bool
depoisHoras4 (H h1 m1) (H h2 m2)
  | h1 > h2 = True
  | h1 < h2 = False
  | otherwise = m1 > m2

-- c)
convHoras4 :: Hora -> Int
convHoras4 (H h m) = h * 60 + m

-- d)
convMinutos4 :: Int -> Hora
convMinutos4 a = H h m
    where h = div a 60
          m = mod a 60

-- e)
difHoras4 :: Hora -> Hora -> Int
difHoras4 (H h1 m1) (H h2 m2) = abs (convHoras4 (H h1 m1) - convHoras4 (H h2 m2))

-- f)
addMinutos4 :: Hora -> Int -> Hora
addMinutos4 (H h m) b =
    let totalMinutos = convHoras4 (H h m) + b
        (horasResultantes, minutosResultantes) = divMod totalMinutos 60
    in H (mod horasResultantes 24) minutosResultantes


-- Ex5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a)
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- b)
stop :: Semaforo -> Bool
stop Vermelho = True
stop a = False

-- c)
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False


-- Ex6
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

-- a)
posx :: Ponto -> Double
posx (Cartesiano x1 y1) = x1
posx (Polar x rad) = abs(x * cos rad)

-- b)
posy :: Ponto -> Double
posy (Cartesiano x1 y1) = y1
posy (Polar x rad) = abs(x * sin rad)

-- c)
raio :: Ponto -> Double
raio (Cartesiano x y) = dist (x,y) (0,0)
raio (Polar r rad) = r

-- d)
angulo :: Ponto -> Double
angulo (Polar r rad) = rad
angulo (Cartesiano x y) = atan y/x

-- e)
dist6 :: Ponto -> Ponto -> Double
dist6 p1 p2 = dist a b
    where a = (posx p1, posy p1)
          b = (posx p2, posy p2)


-- Ex7

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
        deriving (Show,Eq)

pontosDif :: Ponto -> Ponto -> Bool
pontosDif p1 p2 = not (posx p1==posx p2 && posy p1==posy p2)

-- a)
poligono :: Figura -> Bool
poligono (Circulo _ _ ) = False
poligono (Rectangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = posx p1 /= posx p2 || posx p2 /= posx p3 || posx p1 /= posx p3
                                && posy p1 /= posy p2 || posy p2 /= posy p3 || posy p1 /= posy p3

-- b)
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), p2, Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

-- c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist6 p1 p2
        b = dist6 p2 p3
        c = dist6 p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)
area (Circulo _ r) = pi * (r ^ 2)

-- d)
perimetro7 :: Figura -> Double
perimetro7 (Circulo _ r) = 2 * pi * r
perimetro7 (Rectangulo p1 p2) = abs (posx p2 - posx p1) * 2 + abs (posy p2 - posy p1) * 2
perimetro7 (Triangulo p1 p2 p3) = dist6 p1 p2 + dist6 p2 p3 + dist6 p1 p3


-- Ex8

-- a)
isLower :: Char -> Bool
isLower c = ord c >= ord 'a' && ord c <= ord 'z'

-- b)
isDigit :: Char -> Bool
isDigit c = ord c >= ord '0' && ord c <= ord '9'

-- c)
isAlpha :: Char -> Bool
isAlpha c = (ord c >= ord 'a' && ord c <= ord 'z') || (ord c >= ord 'A' && ord c <= ord 'Z')

-- d)
toUpper :: Char -> Char
toUpper c
    | ord c >= ord 'a' && ord c <= ord 'z' = chr (ord c - 32)
    | otherwise = c

-- e)
intToDigit :: Int -> Char
intToDigit n
    | n >= 0 && n <= 9 = chr (ord '0' + n)
    | otherwise = error "intToDigit: entrada fora do intervalo 0-9"

-- f)
digitToInt :: Char -> Int
digitToInt c
    | isDigit c = ord c - ord '0'
    | otherwise = error "digitToInt: caracter não é um dígito"