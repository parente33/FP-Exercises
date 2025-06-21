--1
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y
    | x>y = []
    | otherwise = x : enumFromTo1 (x+1) y

--2
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z
    | x>z && y>=x || x<z && y<x = []
    | otherwise = x : enumFromThenTo1 y (2*y-x) z

--3
(+++) :: [a] -> [a] -> [a]
(+++) l [] = l
(+++) [] l = l
(+++) (x:xs) (y:ys) = x : (+++) xs (y:ys)

--4
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n | n > 0 = xs !!! (n-1)

--5
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

--6
take1 :: Int -> [a] -> [a]
take1 n (h:t)
    | n<=0 = []
    | n<length (h:t) && n>0  = h : take1 (n-1) t
    | n>=length (h:t) && n>0 = h:t

--7
drop1 :: Int -> [a] -> [a]
drop1 n (h:t)
    | n<=0 = h:t
    | n<length (h:t) && n>0 = drop1 (n-1) t
    | n>=length (h:t) && n>0 = []

--8
zip1 :: [a] -> [b] -> [(a,b)]
zip1 _ [] = []
zip1 [] _ = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

--9
replicate1 :: Int -> a -> [a]
replicate1 n x
    | n<=0 = []
    | otherwise = x : replicate1 (n-1) x

--10
intersperse1 :: a -> [a] -> [a]
intersperse1 _ [] = []
intersperse1 _ [x] = [x]
intersperse1 a (x:t) = x : a : intersperse1 a t

--11
group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [x] = [[x]]
group1 (h:t)
    | elem h (head r) = (h:head r) : tail r
    | otherwise = [h] : r
    where r = group1 t

--12
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 [[x]] = [x]
concat1 (h:t) = h ++ concat1 t

--13
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 (x:xs) = inits1 (init (x:xs)) ++ [x:xs]

--14
tails1 :: [a] -> [[a]]
tails1 [] = [[]]
tails1 l = l : tails1 (tail l)

--15
heads1 :: [[a]] -> [a]
heads1 [] = []
heads1 ([]:t) = heads1 t
heads1 (x:xs) = head x : heads1 xs

--16
total1 :: [[a]] -> Int
total1 [] = 0
total1 (x:xs) = length x + total1 xs

--17
fun1 :: [(a,b,c)] -> [(a,c)]
fun1 [] = []
fun1 ((a,b,c):t) = (a,c) : fun1 t 

--18
cola1 :: [(String,b,c)] -> String
cola1 [] = []
cola1 ((str,b,c):t) = str ++ cola1 t

--19
idade1 :: Int -> Int -> [(String,Int)] -> [String]
idade1 _ _ [] = []
idade1 ano idade ((nome,nasc):t)
    | ano-nasc >= idade = nome : idade1 ano idade t
    | otherwise = idade1 ano idade t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom 0 0 = []
powerEnumFrom _ 1 = [1]
powerEnumFrom n m = powerEnumFrom n (m-1) ++ [n^(m-1)]

--21
isPrime :: Int -> Bool
isPrime n = n>=2 && verPrimo n 2
    where verPrimo n m 
            | m*m > n = True
            | mod m n == 0 = False
            | otherwise = verPrimo n (m+1)

--22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (x:xs)
    | h==x && isPrefixOf t xs = True
    | otherwise = False

--23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf (h:t) (x:xs)
    | (h:t)==(x:xs) || isSuffixOf (h:t) xs = True
    | otherwise = False

--24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (x:xs) = h==x && isSubsequenceOf t xs || isSubsequenceOf (h:t) xs

--25
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = igualAux x l 0
    where igualAux _ [] _ = []
          igualAux x l@(h:t) i
            | x==h = i:igualAux x t (i+1)
            | otherwise = igualAux x t (i+1)

--26
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t)
    | elem h t = nub t
    | otherwise = h: nub t

--27
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t)
    | x==h = t
    | otherwise = h:delete x t

--28
(\\) :: Eq a => [a] -> [a] -> [a] 
(\\) l [] = l
(\\) [] _ = []
(\\) (h:t) (h1:t1) = (\\) (delete h1 (h:t)) t1

--29
union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union [] l = l
union (h:t) (x:xs)
    | x `elem` (h:t) = union (h:t) xs
    | otherwise = union ((h:t)++[x]) xs

--30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] l = []
intersect (h:t) l
    | h `elem` l = h:intersect t l
    | otherwise = intersect t l

--31
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t)
    | x==h || x<h = x:h:t
    | x>h = h:insert x t

--32
unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h ++ (if null t then "" else " ") ++ unwords1 t

--33
unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h++"\n" ++ unlines1 t

--34
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t)
    | h>=(t !!! x) = 0
    | otherwise = 1+x
    where x = pMaior t 

--35
lookup1 :: Eq a => a -> [(a,b)] -> Maybe b
lookup1 _ [] = Nothing
lookup1 char ((a,b):t)
    | char==a = Just b
    | otherwise = lookup1 char t

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (a:b:t)
    | b>=a = a : preCrescente (b:t)
    | otherwise = [a]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

--38
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (h:t) (x:xs)
    | h==x = menor t xs
    | h>x = False
    | otherwise = True

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((a,b):t)
    | x==a = True
    | otherwise = elemMSet x t

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,1):t) = a : converteMSet t
converteMSet ((a,b):t) = a : converteMSet ((a,b-1):t)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = []
insereMSet x ((a,b):t)
    | x==a = (a,b+1) :t
    | otherwise = (a,b) : insereMSet x t

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t) 
    | x==a = if b>1 then (a,b-1) : t else t
    | otherwise = (a,b) : removeMSet x t

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)

--44
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left b):t) = (b: as,bs)
    where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b:bs)
    where (as,bs) = partitionEithers t

--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a:t) = a:catMaybes t
catMaybes (Nothing:t) = catMaybes t


data Movimento = Norte | Sul | Este | Oeste
    deriving Show
--46
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xi>xf = Oeste : caminho (xi-1,yi) (xf,yf)
    | xi<xf = Este : caminho (xi+1,yi) (xf,yf)
    | yi<yf = Norte : caminho (xi,yi+1) (xf,yf)
    | yi>yf = Sul : caminho (xi,yi-1) (xf,yf)
    | otherwise = []

--47
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t
posicao (x,y) (Este:t) = posicao (x+1,y) t

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops posi movs = posi == posicao posi movs || hasLoops posi (init movs)


type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto
--48
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x1,y1) (x2,y2)):t)
    | abs (x1-x2) == abs (y1-y2) = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x1-x2) * abs(y1-y2) + areaTotal t


data Equipamento = Bom | Razoavel | Avariado
    deriving Show
--50
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t