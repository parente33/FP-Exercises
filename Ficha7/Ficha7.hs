module Ficha7 where

-- Ex1

data ExpInt = Const Int
    | Simetrico ExpInt
    | Mais ExpInt ExpInt
    | Menos ExpInt ExpInt
    | Mult ExpInt ExpInt

-- a)
calcula :: ExpInt -> Int
calcula (Const num) = num
calcula (Simetrico exp) = - calcula exp
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b

-- b)
infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "-" ++ infixa e
infixa (Mais e1 e2) = '(':infixa e1++"+"++infixa e2++")"
infixa (Menos e1 e2) = '(':infixa e1++"-"++infixa e2++")"
infixa (Mult e1 e2) = "("++infixa e1++"*"++infixa e2++")"

-- c)
posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = posfixa e++"-"
posfixa (Mais e1 e2) = posfixa e1++" "++posfixa e2++" +"
posifxa (Menos e1 e2) = posfixa e1++" "++posfixa e2++" -"
posifxa (Mult e1 e2) = posfixa e1++" "++posfixa e2++" *"


-- Ex2

data RTree a = R a [RTree a]
    deriving (Show)

arv1 = R 5 [ R 4 [ R 3 [R 17 []], R 2 [], R 7 []],
            R 10 [],
            R 1 [ R 8 [ R 0 [], R 20 [], R 15 [], R 39 [] ],
                R 12 [] ]
            ]           

-- a)
soma :: Num a => RTree a -> a
soma (R x lista) = x + sum (map soma lista)

-- b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x lista) = 1 + maximum (map altura lista)

-- c)
prune :: Int -> RTree a -> RTree a
prune n (R x lista)
    | n==1 = R x []
    | n>1 = R x (map (prune (n-1)) lista)

-- d)
mirror :: RTree a -> RTree a
mirror (R e es) = R e (map mirror (reverse es))

-- e)
postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e es) = concatMap postorder es ++ [e]


-- Ex3

data BTree a = Empty
    | Node a (BTree a) (BTree a)
        deriving (Show)


{-
pruneBT :: Int -> BTree a -> BTree a
pruneBT n Empty = Empty
pruneBT n (Node x e d)
    | n<=0 = Empty
    | otherwise = Node x (pruneBT (n-1) e) (pruneBT (n-1) d)
-}


data LTree a = Tip a
                | Fork (LTree a) (LTree a)
        deriving (Show)

ltree1 = Fork (Tip 3) (Fork (Fork (Tip 7) (Tip 8)) (Tip 4))

-- a)
ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

-- b)
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

-- c)
ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)


-- Ex4

data FTree a b = Leaf b
                | No a (FTree a b) (FTree a b)
        deriving (Show)

ftree1 = No 1 (Leaf 'A') (No 7 (Leaf 'B') (Leaf 'C'))

-- a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No x e d) = (Node x be bd , Fork le ld)
    where (be,le) = splitFTree e
          (bd,ld) = splitFTree d

-- b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) =
    case (joinTrees l a, joinTrees r b) of (Just x, Just y) -> Just (No e x y)
                                           _ -> Nothing
joinTrees _ _ = Nothing
