module Ficha8 where
import Data.List (sortBy)

-- Ex1

data Frac = F Integer Integer

frac1 = F (-33) (-51)
frac2 = F 50 (-5)

-- a)
normaliza :: Frac -> Frac
normaliza (F x y) = F (s*a) b
    where d = mdc (abs x) (abs y)
          a = div (abs x) d
          b = div (abs y) d
          s = signum (x*y)

mdc :: Integer -> Integer -> Integer
mdc a b -- a,b positivos
    | a>b = mdc (a-b) b
    | a<b = mdc a (b-a)
    | otherwise = a

-- b)
instance Eq Frac where
    (F a b) == (F c d) = a*d == b*c

-- c)
instance Ord Frac where
    f1 <= f2 = let (F a b) = normaliza f1
                   (F c d) = normaliza f2
               in a*d <= b*c

-- d)
instance Show Frac where
    show :: Frac -> String
    show (F a b) = "("++show a++"/"++show b++")"

{-   
    show f = show a ++ "/" ++ show b
        where F a b = normaliza f
-}

-- e)
instance Num Frac where
    (+), (*), (-) :: Frac -> Frac -> Frac
    (F a b) + (F c d) = F (a*d + c*d) (b*d)
    (F a b) - (F c d) = F (a*d - c*d) (b*d)
    (F a b) * (F c d) = F (a*c) (b*d)

    abs :: Frac -> Frac
    abs (F a b) = F (abs a) (abs b)

    signum :: Frac -> Frac
    signum (F a b) = F (signum (a*b)) 1

    fromInteger :: Integer -> Frac
    fromInteger x = F x 1

-- f)
fun :: Frac -> [Frac] -> [Frac]
fun f l = filter (>2*f) l


-- Ex2

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- (3+(2-5))
exp1 = Mais (Const 3) (Menos (Const 2) (Const 5))

-- ((2-3)*-(5+20))
exp2 = Mult (Menos (Const 2) (Const 3))
            (Simetrico (Mais (Const 5) (Const 20)))

-- a)
instance (Show a) => Show (Exp a) where
    show :: Exp a -> String
    show (Const x) = show x
    show (Mais e1 e2) = "("++show e1++"+"++show e2++")"
    show (Menos e1 e2) = "("++show e1++"-"++show e2++")"
    show (Mult e1 e2) = "("++show e1++"*"++show e2++")"
    show (Simetrico e) = "-"++show e

calcula :: Num a => Exp a -> a
calcula (Const x) = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

-- b)
instance (Eq a,Num a) => Eq (Exp a) where
    e1==e2 = calcula e1 == calcula e2

-- c)
instance (Eq a,Num a) => Num (Exp a) where
    (+), (*), (-) :: Exp a -> Exp a -> Exp a

    e1 + e2 = Mais e1 e2 -- (+) = Mais
    (*) = Mult 
    e1 - e2 = Menos e1 e2 -- (-) = Menos || (-) e1 e2 = Menos e1 e2

    signum :: Exp a -> Exp a
    signum e = Const (signum (calcula e))

    abs :: Exp a -> Exp a
    abs e = if signum e == -1
                then Simetrico e
                else e

    fromInteger :: Integer -> Exp a
    fromInteger x = Const (fromInteger x)


-- Ex 3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

-- a)
instance Eq Data where
    (D d1 m1 a1) == (D d2 m2 a2) = (a1, m1, d1) == (a2, m2, d2)

instance Ord Data where
    compare (D d1 m1 a1) (D d2 m2 a2) = compare (a1, m1, d1) (a2, m2, d2)

-- b)
instance Show Data where
    show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d

-- c)
ordena :: Extracto -> Extracto
ordena (Ext saldo movs) = Ext saldo (sortBy (\(d1,_,_) (d2,_,_) -> compare d1 d2) movs)

-- d)
instance Show Extracto where
    show ext@(Ext saldo movs) =
        let Ext _ ordMovs = ordena ext
            linha (data_, desc, Credito v) = show data_ ++ "  " ++ ajusta desc ++ ajustaF v ++ "   "
            linha (data_, desc, Debito v)  = show data_ ++ "  " ++ ajusta desc ++ "      " ++ ajustaF v
            ajusta s = take 10 (s ++ replicate 10 ' ')
            ajustaF x = let s = show x in s ++ replicate (10 - length s) ' '
            corpo = unlines (map linha ordMovs)
            total = calcSaldo saldo movs
        in  "Saldo anterior: " ++ show saldo ++ "\n" ++
            "---------------------------------------\n" ++
            "Data       Descricao  Credito  Debito\n" ++
            "---------------------------------------\n" ++
            corpo ++
            "---------------------------------------\n" ++
            "Saldo actual: " ++ show total

calcSaldo :: Float -> [(Data, String, Movimento)] -> Float
calcSaldo saldo = foldl aplicaMov saldo
  where
    aplicaMov acc ( _, _, Credito v) = acc + v
    aplicaMov acc ( _, _, Debito  v) = acc - v