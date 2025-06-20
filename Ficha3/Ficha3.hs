module Ficha3 where
import Ficha1

-- Ex1

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- a)
etapaVal :: Etapa -> Bool
etapaVal (p,c) = validaHoras4 p && validaHoras4 c && depoisHoras4 c p

-- b)
viagemVal :: Viagem -> Bool
viagemVal [] = False
viagemVal ((p1,c1):(p2,c2):t) = etapaVal (p1,c1) &&
                                depoisHoras4 p2 c1 &&
                                viagemVal ((p2,c2):t)

-- c)
horaPartCheg :: Viagem -> (Hora,Hora)
horaPartCheg ((p,c):t) = (p, snd (last t))

-- d)
tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem ((p1,c1):t) = tv
        where tv = (convHoras4 c1 - convHoras4 p1) + tempoViagem t

-- e)
tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera ((p1,c1):(p2,c2):(p3,c3):t) = te
        where te = (convHoras4 p2 - convHoras4 c1) + (convHoras4 p3 - convHoras4 c2) + tempoEspera t

-- f)
tempoTotal :: Viagem -> Int
tempoTotal [] = 0
tempoTotal v = tempoEspera v + tempoViagem v


-- Ex2

type Poligonal = [Ponto]

-- a)
compLinha :: Poligonal -> Double
compLinha (p1:p2:t) = dist6 p1 p2 + compLinha (p2:t)
compLinha _ = 0

-- b)
linhaFech :: Poligonal -> Bool -- se a linha for fechada, termina aí - não contabiliza os casos em que a linha continua depois de "terminar"
linhaFech [] = False
linhaFech (p1:t)
        | length (p1:t) >= 3 && p1==last t = True
        | otherwise = False

linhaFech2 :: Poligonal -> Bool -- função completa
linhaFech2 [] = False
linhaFech2 (p1:t) = (pontoAux p1 t  >= 1) || linhaFech2 t
        where pontoAux p [] = 0
              pontoAux p (h:t)
                | p == h = 1 + pontoAux p t
                | otherwise = pontoAux p t

-- c)
triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:ps)
    | p1 == p3 = []
    | otherwise = Triangulo p1 p2 p3 : triangula (p1:p3:ps)
triangula _ = []

-- d)
areaPol :: Poligonal -> Double
areaPol p = areaTris (triangula p)

areaTris :: [Figura] -> Double
areaTris [] = 0
areaTris (h:t) = area h + areaTris t

-- e)
mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p : pol

-- f)
zoom :: Double -> Poligonal -> Poligonal
zoom z (h:t) = mover (doZoom z h t) h

doZoom :: Double -> Ponto -> Poligonal -> Poligonal
doZoom z p [] = []
doZoom z p (h:t) = Cartesiano ((x - xp) * z + xp) ((y - yp) * z + yp) : doZoom z p t
    where x = posx h
          y = posy h
          xp = posx p
          yp = posy p


-- Ex3

data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email String
                deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome,[Email email])]
acrescEmail nome email ((n,cs):t)
    | nome==n = (n,Email email:cs):t
    | otherwise = (n,cs):acrescEmail nome email t

-- b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,l):t)
        | n==x = Just (procEmails l)
        | otherwise = verEmails n t

procEmails :: [Contacto] -> [String]
procEmails ((Email s):t) = s : procEmails t
procEmails (h:t) = procEmails t
procEmails [] = []

-- c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa n):t) = n:consTelefs t
consTelefs ((Trab n):t) = n:consTelefs t
consTelefs ((Tlm n):t) = n:consTelefs t
consTelefs (_:t) = consTelefs t

-- d)
casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((n,cs):t)
    | nome==n = numCasa cs
    | otherwise = casa nome t

numCasa :: [Contacto] -> Maybe Integer
numCasa [] = Nothing
numCasa (Casa n:t) = Just n
numCasa (_:t) = numCasa t


-- Ex4

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String
data Data = D Dia Mes Ano
        deriving Show
type TabDN = [(Nome,Data)]

-- a)
procura :: Nome -> TabDN -> Maybe Data
procura pessoa [] = Nothing
procura pessoa ((nome,D dia mes ano):t)
        | pessoa == nome = Just (D dia mes ano)
        | otherwise = procura pessoa t

-- b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade d p [] = Nothing
idade d p tab = case procura p tab of
                         Nothing -> Nothing
                         Just x -> Just (calcula d x)

calcula :: Data -> Data -> Int
calcula (D dh mh ah) (D dn mn an)
        | (mh > mn) || (mh==mn && dh==dn) = ah - an
        | (mh==mn && dh<dn) || mh<mn = ah-an

idade' :: Data -> Nome -> TabDN -> Maybe Int
idade' _ _ [] = Nothing
idade' (D dx mx ax) nome ((n,D d m a):ts) 
    | nome == n = Just (calculaIdade (D d m a) (D dx mx ax))
    | otherwise = idade (D dx mx ax) nome ts

calculaIdade :: Data -> Data -> Int
calculaIdade (D dn mn an) (D d m a) = if m > mn || m == mn && d > dn then a - an else a - an - 1

-- c)
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2)
        | a1>a2 || (a1==a2 && m1>m2) || (a1==a2 && m1==m2 && d1>d2) = False
        | a1<a2 || (a1==a2 && m1<m2) || (a1==a2 && m1==m2 && d1<d2) = True

anterior' :: Data -> Data -> Bool
anterior' (D d m a) (D d2 m2 a2) = a < a2 || (a == a2 && (m < m2 || (m == m2 && d < d2)))

-- d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = insere h (ordena t)

insere :: (Nome,Data) -> TabDN -> TabDN
insere (n,d) [] = [(n,d)]
insere (n,d) ((n1,d1):t)
        | anterior d d1 = (n,d):(n1,d1):t
        | otherwise = (n1,d1) : insere (n,d) t

-- e)
porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade datah [] = []
porIdade datah ((n,d):t) = aux d (reverse (ordena ((n,d):t)))

aux :: Data -> TabDN -> [(Nome,Int)]
aux _ [] = []
aux datah ((n,d):t) = (n,calcula datah d) : aux datah t


-- Ex5

data Movimento = Credito Float | Debito Float
        deriving Show
data Data' = D' Int Int Int
        deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
        deriving Show

-- a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext si ((_,_,mov):t)) valor = if getValor mov > valor then mov : extValor (Ext si t) valor else extValor (Ext si t) valor

getValor :: Movimento -> Float
getValor (Credito x) = x
getValor (Debito x) = x

-- b)
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext si ((dataa,desc,mov):t)) listaStr = if desc `elem` listaStr then (dataa,mov) : filtro (Ext si t) listaStr else filtro (Ext si t) listaStr

-- c)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext si ((_,_,Credito x):t)) = (x + cr, dr)
    where (cr,dr) = creDeb (Ext si t)
creDeb (Ext si ((_,_,Debito x):t)) = (cr, x + dr)
    where (cr,dr) = creDeb (Ext si t)

creDeb' :: Extracto -> (Float,Float)
creDeb' (Ext _ []) = (0,0)
creDeb' (Ext si ((_,_,mov):t)) = (c + cr, d + dr)
    where (cr,dr) = creDeb (Ext si t)
          (c,d) = case mov of Credito x -> (x,0)
                              Debito x -> (0,x)

-- d)                    
saldo :: Extracto -> Float
saldo (Ext si []) = si
saldo (Ext si ((_,_,Debito x):t)) = saldo (Ext (si + x) t)
saldo (Ext si ((_,_,Credito x):t)) = saldo (Ext (si - x) t)

