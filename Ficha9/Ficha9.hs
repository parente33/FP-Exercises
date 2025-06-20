module Ficha9 where

import System.Random (randomRIO)
import Control.Monad (when)
import Data.List (delete)

-- Ex1

-- a)
bingo :: IO ()
bingo = tiraNum 90 [1..90]

tiraNum :: Int -> [Int] -> IO ()
tiraNum 0 l = do putStrLn "---FIM---"
                 return ()
tiraNum n s = do putStrLn "Prima uma tecla"
                 hSetEcho stdin False
                 getChar 
                 hSetEcho stdin True
                 p <- randomRIO (0,n-1)
                 let (s1,x:s2) = splitAt p s
                 print x
                 tiraNum (n-1) (s1++s2)

-- b)
geraSequencia :: IO [Int]
geraSequencia = sequence $ replicate 4 (randomRIO (0,9))

leTentativa :: IO [Int]
leTentativa = do
    putStrLn "Introduza 4 dígitos (ex: 1234):"
    entrada <- getLine
    if length entrada /= 4 || any (not . (`elem` ['0'..'9'])) entrada
       then do
           putStrLn "Por favor, introduza exatamente 4 dígitos."
           leTentativa
       else return $ map (\c -> fromEnum c - fromEnum '0') entrada

contaCertosNaPosicao :: [Int] -> [Int] -> Int
contaCertosNaPosicao segredo tentativa = length [() | (s,t) <- zip segredo tentativa, s == t]

contaCertosForaDePosicao :: [Int] -> [Int] -> Int
contaCertosForaDePosicao segredo tentativa = go segredo tentativa 0
  where
    go [] _ acc = acc
    go (s:ss) ts acc =
        if s `elem` ts
           then go ss (delete s ts) (acc+1)
           else go ss ts acc

mastermind :: IO ()
mastermind = do
    segredo <- geraSequencia
    putStrLn "Jogo Mastermind iniciado! Tente adivinhar a sequência de 4 dígitos."
    loop segredo

  where
    loop segredo = do
        tentativa <- leTentativa
        let certosNaPos = contaCertosNaPosicao segredo tentativa
            certosForaPos = contaCertosForaDePosicao segredo tentativa - certosNaPos
        putStrLn $ "Corretos na posição certa: " ++ show certosNaPos
        putStrLn $ "Corretos na posição errada: " ++ show certosForaPos
        if certosNaPos == 4
           then putStrLn "Parabéns! Acertou na sequência secreta!"
           else loop segredo


-- Ex2

data Aposta = Ap [Int] (Int,Int)
        deriving (Show)

-- a)
valida :: Aposta -> Bool
valida (Ap lnums (a,b)) = a/=b && a>=1 && a<=9 && b>=1 && b<=9 && length lnums == 5 && valNums lnums

valNums :: [Int] -> Bool
valNums [] = True
valNums (x:xs) = x>=1 && x<=50 && notElem x xs && valNums xs

-- b)
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l1 (a,b)) (Ap l2 (c,d)) = (numComuns l1 l2, numComuns [a,b] [c,d])

numComuns :: [Int] -> [Int] -> Int
numComuns [] l = 0
numComuns (x:xs) l = if elem x l
                     then 1+numComuns xs l
                     else numComuns xs l

-- c) i)
instance Eq Aposta where
    (==) :: Aposta -> Aposta -> Bool
    ap1==ap2 = comuns ap1 ap2 == (5,2)

-- c) ii)
premio :: Aposta -> Aposta -> Maybe Int
premio ap ch = case comuns ap ch of
                (5,2) -> Just 1
                (5,1) -> Just 2
                (5,0) -> Just 3
                (4,2) -> Just 4
                (4,1) -> Just 5
                (4,0) -> Just 6
                (3,2) -> Just 7
                (2,2) -> Just 8
                (3,1) -> Just 9
                (3,0) -> Just 10
                (1,2) -> Just 11
                (2,1) -> Just 12
                (2,0) -> Just 13
                _ -> Nothing

-- d) i)
leAposta :: IO Aposta
leAposta = do putStrLn "Escreva a lista de 5 números (1-50):"
              l <- getLine
              putStrLn "Escreva o par de estrelas (1-9):"
              e <- getLine
              let ap = Ap (read l) (read e)
              if valida ap
                then return ap
                else do putStrLn "A aposta não é válida"
                        leAposta

-- d) ii)
joga :: Aposta -> IO ()
joga ch = do ap <- leAposta
             case premio ap ch of
                Just n -> putStrLn ("Tem o "++show n++"º premio!")
                Nothing -> putStrLn "Não tem prémio :["

-- e)
geraChave :: IO Aposta
geraChave = do nums <- geraNums
               est <- geraEstrelas
               return (Ap nums est)

-- f)
main :: IO ()
main = do ch <- geraChave
          ciclo ch
menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
    where menutxt = unlines ["",
                            "Apostar ........... 1",
                            "Gerar nova chave .. 2",
                            "",
                            "Sair .............. 0"]