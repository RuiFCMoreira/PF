module Teste where

import Data.Char
-- Exercicio 1
enum  :: Int -> Int -> [Int]
enum n m | n<=m = n : enum (n+1) m 
		 | otherwise = []

-- Exercicio 2
enumFromThento2C :: Int -> Int -> Int -> [Int]
enumFromThento2C x y z | x>z = []
					   | otherwise = x : enumFromThento2 y (y+(y-x))  z

--enumFromThento2D :: Int -> Int -> Int -> [Int]
--enumFromThento2D 

-- Exercicio 3
somaListas:: [a] -> [a] -> [a]
somaListas [] l = l
somaListas (x:xs) l = x :somaListas xs l 

-- Exercicio  4
localiza :: [a] -> Int -> a
localiza (x:xs) 0 = x
localiza (x:xs) n = localiza xs (n-1)

-- Exercicio 5 
reverse5 :: [a] -> [a]
reverse5 l = reverseAc l []
		where 
			reverseAc :: [a] -> [a] -> [a]
			reverseAc (x:xs) a = reverseAc xs (x:a)
			reverseAc [] a = a

-- Exercicio 6 
take6 :: Int -> [a] -> [a]
take6 0 l = []
take6 n [] = []
take6 n (x:xs)| n>0 = x : (take6 (n-1) xs)

-- Exercicio 7
drop7 :: Int -> [a] -> [a]
drop7 0 l  = l
drop7 n [] = []
drop7 n (x:xs) | n>0 = 	(drop7 (n-1) xs)

-- Exercicio 8 
zip8 :: [a] -> [b] -> [(a,b)]
zip8 [] l = []
zip8 l [] = []
zip8 (h1:t1) (h2:t2)  = (h1,h2) : zip8 t1 t2

-- Exercicio 9 
elem9 :: Eq a => a -> [a] -> Bool
elem9 y [] = False
elem9 y (x:xs) | x==y = True
			   | otherwise = elem9 y xs

-- Exercicio 10 
replicate10 :: Int -> a -> [a]
replicate10 0 y = []
replicate10 x y = y : replicate10 (x-1) y

-- Exercicio 11
intersperse11 :: a -> [a] -> [a]
intersperse11 y [] = []
intersperse11 y (x:xs) = x : y : intersperse11 y xs

-- Exercicio 12
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h:takeWhile (== h) t) : group' (dropWhile (== h) t)

-- Exercicio 13
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:y) = somaListas x (concat' y)
			where 
				somaListas:: [a] -> [a] -> [a]
				somaListas [] l = l
				somaListas (x:xs) l = x :somaListas xs l 

-- Exercicio 14
--inits' :: [a]->[[a]]
--inits' [] = []
--inits' (x:xs) = reverse5 

-- Exercicio 15 
tails' :: [a]->[[a]]
tails' [] = [[]]
tails' (h:t) = (h:t) : tails' t

-- Exercicio 16 
isPrefixOf':: Eq a=>[a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = True 
isPrefixOf' (x:xs) (y:ys) | x==y = isPrefixOf' xs ys
						  | otherwise = False

-- Exercicio 17 
isSuffixOf':: Eq a=>[a] -> [a] -> Bool
isSuffixOf' _ [] = False 
isSuffixOf' [] _ = True 
isSuffixOf' (x:xs) (y:ys) | x == head ys = isSuffixOf' xs ys
						  | otherwise = False

-- Exercicio 18 
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) | x==y = isSubsequenceOf' xs ys
							   | otherwise = isSubsequenceOf' (x:xs) ys

-- Exercicio 19 
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' y [] = []
elemIndices' y (x:xs) = elemIndicesAux 0 y (x:xs)

elemIndicesAux :: Eq a => Int -> a -> [a] -> [Int]
elemIndicesAux n y [] = []
elemIndicesAux n y (x:xs) | y==x = n: elemIndicesAux (n+1) y xs
						  | otherwise = elemIndicesAux (n+1) y xs

-- Exercicio 20 
nub' :: Eq a => [a] -> [a]
nub' (x:xs) = nubAux  [] (x:xs)  

nubAux :: Eq a => [a] -> [a] -> [a]
nubAux n [] = []
nubAux n (h:t) | (elem' h n ==True) = nubAux n t 
			   | otherwise = h : nubAux (h:n) t

elem' :: (Eq a) => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) | y==x = True
 			   | otherwise = elem' y xs

-- Exercicio 21
delete' :: Eq a => a -> [a] -> [a]
delete' y [] = []
delete' y (x:xs) | y/=x = x : delete' y xs
		   		 | otherwise = xs

-- Exercicio 22 
(\\) :: Eq a=>[a]->[a] -> [a]
(\\) l [] = []
(\\) l (x:xs) = delete' (head xs) (delete' x l) 

-- Exercicio 23
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (y:ys) | (elem' y l == True) = union' l ys 
			    | otherwise = union' (l ++ [y]) ys

-- Exercicio 24
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' (x:xs) l | (pertence x l== True) = x : intersect' xs l
				    | otherwise = intersect' xs l 


pertence :: Eq a => a -> [a] -> Bool
pertence y [] = False
pertence y (x:xs) | y==x = True
				  | otherwise = pertence y xs

-- Exercicio 25 
insert' :: Ord a => a -> [a] -> [a]
insert' c [] = [c]
insert' c (x:xs) | c>x = x: (insert' c xs)
				 | otherwise = (c:x:xs) 

-- Exercicio 26
unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (h:t) = h ++" "++ unwords' t

-- Exercicio 27
unlines' :: [String] ->  String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

-- Exercicio 28
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = aux 0 0 h t

aux :: Ord a => Int -> Int -> a -> [a] ->  Int
aux p pm x [] = pm 
aux p pm x (h:t) | x>=h = aux (p+1) pm x t 
				 | otherwise = aux (p+1) (p+1) h t

-- Exercicio 29 
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:xs) | (elem x xs==True) = True
					| otherwise = temRepetidos xs

-- Exercicio 30 
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | h `elem` ['0'..'9'] = h:algarismos t
    			 | otherwise = algarismos t

--Exercicio  31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [_] = []
posImpares (x:y:xs) = y : posImpares xs

-- Exercicio 32
posPares  :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:xs) = x : posPares xs

-- Exercicio 33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x <= y = isSorted (y:xs)
				  |  otherwise = False

-- Exercicio 34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)

-- Exercicio 35
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t') = h < h' || menor t t'

-- Exercicio 36 (ERRO)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet y [] = False
elemMSet y ((x,xs):t) | y==x = True
					  | otherwise = elemMSet y t				  

-- Exercicio 37 
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,n):xs) = n + lengthMSet xs

--Exercicio 38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet (h:t) = convertMSetAux h ++ convertMSet t

convertMSetAux :: (a,Int) -> [a]
convertMSetAux (_,0) = []
convertMSetAux (y,x) = y : convertMSetAux (y,(x-1))

--Exercicio 39 
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet y [] = [(y,1)]
insereMSet y ((x,xs):t) | y==x = (x,(xs+1)) : t
						| otherwise = insereMSet y t 

-- Exercicio 40 (Perguntar se é assim?)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet y  [] = []
removeMSet y ((x,xs):t) | y==x = if (xs-1)>0 then (x,(xs-1)) : t   else t
						| otherwise = (x,xs) : removeMSet y t

-- Exercicio 41 
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet l = constroiMSetAux l 0

constroiMSetAux :: Ord a => [a] -> Int -> [(a,Int)]
constroiMSetAux [x] n = [(x,n+1)]
constroiMSetAux [] n = []
constroiMSetAux (x:y:t) n | x==y = constroiMSetAux (y:t) (n+1) 
     					  | otherwise = (x,n) : constroiMSetAux (y:t) 0 

-- Exercicio 42
-- partitionEithers :: [Either a b] -> ([a],[b])


--Exercicio 43
catmaybe :: [Maybe a] -> [a]
catmaybe [] = []
catmaybe (Just x:t) = x: catmaybe t
catmaybe (Nothing : t) = catmaybe t

-- Exercicio 44
data Movimento = Norte | Sul | Este | Oeste
                  deriving Show

posicao:: (Int,Int)->[Movimento]->(Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:t) = posicao(x,(y+1)) t
posicao (x,y) (Sul:t)   = posicao(x,(y-1)) t
posicao (x,y) (Este:t)  = posicao((x+1),y) t
posicao (x,y) (Oeste:t) = posicao((x-1),y) t

-- Exercicio 45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (xs,ys) | (ys-y)>0 = Norte : caminho (x,(y+1)) (xs,ys)
					  | (ys-y)<0 = Sul : caminho (x,(y-1)) (xs,ys)
					  | (xs-x)>0 = Este : caminho ((x+1),y) (xs,ys)
			 		  | (xs-x)<0 = Oeste : caminho ((x-1),y) (xs,ys)
					  | otherwise = []

-- Exercicio 46
vertical :: [Movimento] -> Bool
vertical []        = True
vertical (Sul:t)   = vertical t
vertical (Norte:t) = vertical t
vertical (Este:t)  = False
vertical (Oeste:t) = False

-- Exercicio 47
data Posicao = Pos Int Int
             deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (x:y:t) | (distOrigem x)<= (distOrigem y) = maisCentral (x:t)
					| otherwise = maisCentral (y:t) 

distOrigem :: Posicao -> Int
distOrigem (Pos x y) = (x^2 + y^2) 

-- Exercicio 48 
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos p [] = []
vizinhos (Pos a b) ((Pos x y):t) | (x-a) == 1 || (x-a) == -1 = (Pos x y) : vizinhos (Pos a b) t
								 | (y-b) == 1 || (y-b) == -1 = (Pos x y) : vizinhos (Pos a b) t
								 | otherwise = vizinhos (Pos a b) t

-- Exercicio 49 
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [x] = True 
mesmaOrdenada ((Pos x y):(Pos a b):t) | y==b = mesmaOrdenada ((Pos a b):t)
									  | otherwise = False


-- Exercicio 50 
data Semaforo = Verde | Amarelo | Vermelho
                   deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l | nVA l 0 < 2 = True
			    | otherwise = False


nVA :: [Semaforo] -> Int -> Int
nVA [] n = n
nVA (Vermelho:t) n = nVA t n
nVA (_:t) n = nVA t (n+1)





