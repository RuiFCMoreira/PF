import Data.Char 
-- Exercicio 1
-- a) 



-- c)
-- [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
umc :: [Int] -> [Int] -> [(Int,Int)]
umc [] ys = []
umc (x:xs) ys = junta x ys ++ umc xs ys

junta :: Int -> [Int] -> [(Int,Int)]
junta x [] = []
junta x (y:ys) = (x,y) :  junta x ys

-- Exercicio 3 
digitAlpha :: String -> (String,String)
digitAlpha s = (digits s,alphas s)

digits :: String -> String
digits [] = []
digits (c:cs) | isDigit c = c : digits cs
			  | otherwise = digits cs

alphas :: String -> String
alphas [] = []
alphas (c:cs) | isAlpha c = c : alphas cs 
			  | otherwise = alphas cs


digitAlpha2 :: String -> (String,String)
digitAlpha2 [] = ([],[])
digitAlpha2 (c:cs) | isDigit c = (c:ds,as)
				   | isAlpha c = (ds,c:as)
				   | otherwise = (ds,as) 
	where
		(ds,as) = digitAlpha2 cs 

digitAlpha3 :: String -> (String,String)
digitAlpha3 s= digitAlpha3Acc ([],[]) s 

digitAlpha3Acc :: (String,String) ->  String -> (String,String)
digitAlpha3Acc (ds,as) [] = (ds,as)
digitAlpha3Acc (ds,as) (c:cs) | isDigit c = digitAlpha3Acc (ds++[c],as) cs
							  | isAlpha c = digitAlpha3Acc (ds,as++[c]) cs
							  | otherwise = digitAlpha3Acc (ds,as) cs

-- Exercicio 4 
nzp :: [Int] -> (Int,Int,Int)
nzp s = nzpAcc (0,0,0) s 

nzpAcc :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
nzpAcc (n,z,p) [] = (n,z,p)
nzpAcc (n,z,p) (c:cs) | c<0 = nzpAcc (n+1,z,p) cs
					  | c==0 = nzpAcc (n,z+1,p) cs
					  | c>0 = nzpAcc (n,z,p+1) cs


-- Exercicio 5
divMod5 :: Integral a => a -> a -> (a, a)
divMod5 x y = divModAcc (0,0) x y

divModAcc :: Integral a => (a,a) -> a -> a -> (a, a)
divModAcc (d,r) x y | (x-y) >= 0  = divModAcc (d+1,r) (x-y) y
					| x == y = (d,r)
					| (x-y) < 0 = (d,x)

-- Exercicio 6 
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t


