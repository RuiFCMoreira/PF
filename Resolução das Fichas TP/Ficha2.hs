-- Exercicio 1
-- a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

-- funA [2,3,5,1]= 39
-- funA (2:[3,5,1])
-- 2^2 + funA [3,5,1]
-- 4 + 3^2 + funA [5,1]
-- 4 + 9 + 5^2 + funA [1]
-- 4 + 9 + 25 + funA (1:[])
-- 4 + 9 + 25 + 1 + 0



-- b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 
       		 then h : (funB t)
	   		 else (funB t) 

-- funB [8,5,12] = [8,12]
-- funB (8:[5,12])
-- 8 : funB (5:12)
-- 8 : 12

-- c) 
 --funC (x:y:t) = funC t
 --funC [x] = []
 --funC [] = []

 -- funC [1,2,3,4,5] =
 --funC (1:2:[3,4,5])

 -- d)
funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

-- funD "otrec"
-- g [] "otrec"
-- g ('o' : []) "trec"
-- g ('t' : 'o' : []) "rec"
-- g ('r' : 't' : 'o' : []) "ec"
-- g ('e' : 'r' : 't' : 'o' : []) "c"
-- g ('o' : 'e' : 'r' : 't' : 'o' : [])
-- g "certo" []
-- "certo"

-- Exercicio 2
-- a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = (2*x : dobros xs)

-- b)
numOcorre :: Char -> String -> Int
numOcorre a [] = 0
numOcorre a (x:xs) | a==x = 1 + numOcorre a (xs)
				   | otherwise = numOcorre a (xs) 

-- c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) = if (x >= 0 )
				   then positivos xs
				   else False

-- d) 
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if x>=0
			   then (x : soPos xs)
			   else soPos xs

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) = if x<0
				 then x + (somaNeg xs) 
				 else somaNeg xs

-- f)
tresUlt :: [a] -> [a]
tresUlt (x:y:z) = undefined


-- g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):xs) = (b: segundos xs)

-- h) 
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((a,b):xs) = if x==a 
							then True 
							else nosPrimeiros x xs

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos (x:xs) = somaTriplos x (sumTriplos xs)
somaTriplos (a,b,c) (x,y,z) = (a+x, b+y, c+z)


