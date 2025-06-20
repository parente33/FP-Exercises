module FIcha6 where

-- Ex1

data BTree a = Empty | Node a (BTree a) (BTree a)
        deriving Show

-- a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

-- b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d

-- c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

-- d)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node n e d) = Node n (prune (x-1) e) (prune (x-1) d)

-- e)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node a _ _) = []
path (h:t) (Node a e d) = a : path t (if h
                                        then d
                                        else e)

-- f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a e d) = Node a (mirror d) (mirror e)

-- g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a e d) (Node a' e' d') = Node (f a a') (zipWithBT f e e') (zipWithBT f d d')
zipWithBT _ _ _ = Empty

-- h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a unzipE1 unzipD1,Node b unzipE2 unzipD2,Node c unzipE3 unzipD3)
    where (unzipE1,unzipE2,unzipE3) = unzipBT e
          (unzipD1,unzipD2,unzipD3) = unzipBT d


-- Ex2

-- a)
minimo :: Ord a => BTree a -> a
minimo (Node e Empty _) = e
minimo (Node e l r) = minimo l

-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty r) = r
semMinimo (Node e l r) = Node e (semMinimo l) r

-- c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty r) = (e,r)
minSmin (Node e l r) = (a,Node e b r)
    where (a,b) = minSmin l

-- d)
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r)
    | x<e = Node e (remove x l) r
    | x>e = Node e l (remove x r)
    | otherwise = case r of
                    Empty -> l
                    _ -> let (g,h) = minSmin r
                         in Node g l h


-- Ex3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String

data Regime = ORD | TE | MEL deriving Show

data Classificacao = Aprov Int | Rep | Faltou
        deriving Show

type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

turma:: Turma
turma = Node (15,"Luís",ORD,Aprov 14)
        (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty))
        (Node (20,"Pedro",TE,Aprov 10) Empty
            (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty)
                                            (Node (28,"Vasco",MEL,Rep) Empty Empty)))

-- a)
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) l r) = n == num || inscNum n (if n<num then l else r)

-- b)
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nom,_,_) l r) = n == nom || inscNome n l || inscNome n r

-- c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,TE,_) l r) = trabEst l ++ [(num,nom)] ++ trabEst r
trabEst (Node _ l r) = trabEst l ++ trabEst r

-- d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (num,_,_,classif) l r)
    | n==num = Just classif
    | n<num = nota n l
    | otherwise = nota n r
nota _ _ = Nothing

-- e)
percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = sumFaltas turma / numAlunos turma *100
    where sumFaltas :: Turma -> Float
          sumFaltas Empty = 0
          sumFaltas (Node (_,_,_,Faltou) l r) = 1 + sumFaltas l + sumFaltas r
          sumFaltas (Node _ l r) = sumFaltas l + sumFaltas r
          
          numAlunos = fromIntegral . contaNodos

-- f)
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = uncurry (/) (sumNumNotas turma)
    where sumNumNotas :: Turma -> (Float,Float)
          sumNumNotas Empty = (0,0)
          sumNumNotas (Node (_,_,_,Aprov nota) l r) = addPairs (fromIntegral nota, 1) (addPairs (sumNumNotas l) (sumNumNotas r))
          sumNumNotas (Node _ l r) = addPairs (sumNumNotas l) (sumNumNotas r)
          
          addPairs (a,b) (c,d) = (a+c,b+d)

-- g)
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = uncurry (/) (sumAprovAv turma)

sumAprovAv :: Turma -> (Float,Float)
sumAprovAv Empty = (0,0)
sumAprovAv (Node (_,_,_,clas) l r) = case clas of
                                        Aprov nota -> (ap+1,av+1)
                                        Rep -> (ap,av+1)
                                        _ -> (ap,av)
    where (ap,av) = addPairs (sumAprovAv l) (sumAprovAv r)
          addPairs (a,b) (c,d) = (a+c,b+d)