module Ficha1 where
import Data.Char
-- Exercício 1
-- a)
perimetro :: Double -> Double
perimetro r = 2*pi*r

-- b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (p1x,p1y) (p2x,p2y) =  sqrt (dx+dy)
      where 
        dx = abs (p1x-p2x)^2
        dy = abs (p1y-p2y)^2

-- c)
primUlt :: [Int] -> (Int,Int)
primUlt l = (head l, last l)   

-- d)
multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

-- e)
truncaImpar :: [Double] -> [Double]
truncaImpar l = if odd (length l)
                then tail l 
                else l 

-- f)
max2 :: Int -> Int -> Int
max2 x y = if x > y 
            then x
            else y

-- g)
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z 

-- Exercício 2
-- a)
nraizes :: Double -> Double -> Double -> Int
nraizes a b c = if det<0 
                then 0
                else  if det==0 
                      then 1 
                      else 2
                where 
                    det = b^2 - 4*a*c 

-- b)
raizes :: Double -> Double -> Double -> [Double]
raizes a b c = if n==0 
               then []
               else if n==1 
                    then [r1]
                    else [r2]
    where 
      n = nraizes a b c 
      det = b^2 - 4*a*c 
      r1 = (-b + sqrt det) / (2*a)
      r2 = (-b - sqrt det) / (2*a)        

-- Exercício 3
-- a)
type Hora = (Int,Int)
horaValida :: Hora -> Bool
horaValida (h,m) = (0<=h && h<24) &&  (0<= m && m<60)

-- b)
eDepois :: Hora -> Hora -> Bool
eDepois (h1,m1) (h2,m2) = if  h1>h2 
							then True 
							else if h1<h2 
						 			then False
						  	   		else if m1>m2
						  	        	then True
						  	        	else False 

eDepois (h1,m1) (h2,m2) | h1 > h2 = True
						| h1 < h2 = False
						| otherwise = m1>m2
-- c)						
type Minutos = Int

converteMinutos :: Hora -> Minutos
converteMinutos (h,m) = h * 60 + m

-- d)
converteHoras :: Minutos -> Hora
converteHoras mins = (div mins 60, mod mins 60)

-- e)
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras h1 h2 = abs (converteMinutos h1 - converteMinutos h2)

-- f)
adicionaMinutos:: Hora -> Minutos -> Hora
adicionaMinutos h m = converteHoras (converteMinutos h + m)

-- Exercício4
-- a)
data Horas = H Int Int deriving (Show,Eq)
horaValida4 :: Horas -> Bool
horaValida4 (H h m)= (0<=h && h<24) &&  (0<= m && m<60)

-- b)
eDepois4 :: Horas -> Horas -> Bool
eDepois4 (H h1 m1) (H h2 m2) =  if  h1>h2 
								then True 
								else if h1<h2 
						 			 then False
						  	   		 else if m1>m2
						  	        	  then True
						  	        	  else False  

-- c)
converteMinutos4 :: Horas -> Minutos
converteMinutos4 (H h m) = h * 60 + m	

-- d)
converteHoras4 :: Minutos -> Horas
converteHoras4 mins = H (div mins 60) (mod mins 60)

-- e)
diferencaHoras4 :: Horas -> Horas -> Int
diferencaHoras4 (H h1 m1) (H h2 m2) = abs (converteMinutos4 (H h1 m1) - converteMinutos4 (H h2 m2))

-- f)
adicionaMinutos4 :: Horas -> Minutos -> Horas
adicionaMinutos4 (H h m) m1 = converteHoras4 (converteMinutos4 (H h m) + m1)

-- Exercício 5 
data Semaforo = Verde | Amarelo | Vermelho  deriving (Show,Eq)

-- a)
next :: Semaforo -> Semaforo
next a  |a== Verde = Amarelo
		|a== Amarelo = Vermelho
		|a== Vermelho = Verde

-- b)
stop :: Semaforo -> Bool
stop a  |a== Vermelho = True 
		|a== Amarelo =  True
		|a== Verde = False 

-- c)
safe :: Semaforo -> Semaforo -> Bool
safe a b |(a== Verde && b== Vermelho) =	True
		 |(a== Vermelho && b== Verde) = True 
		 |otherwise = False	

-- Exercicio 6
data Ponto = Cartesiano Double Double | Polar Double Double    deriving (Show,Eq)
-- a)
posx :: Ponto -> Double
posx (Cartesiano x y )= x
posx (Polar r a)= r * cos a

--  b)
posy :: Ponto -> Double 
posy (Cartesiano x y)= y
posy (Polar r a)= r * sin a

-- c)
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt a
			where
				a = (posx (Cartesiano x y))^2 + (posy (Cartesiano x y))^2
raio (Polar r a) = r

-- d)
angulo :: Ponto -> Double
angulo (Cartesiano x y) = undefined
angulo (Polar r a) = a

--  e)
dist6 :: Ponto -> Ponto -> Double
dist6 (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt (((x2-x1)^2)+((y2-y1)^2)) 
dist6 (Polar r1 a1) (Polar r2 a2) = sqrt (((x2-x1)^2)+((y2-y1)^2))
				where
					x1 = posx (Polar r1 a1)
					x2 = posx (Polar r2 a2)
					y1 = posy (Polar r1 a1)
					y2 = posy (Polar r2 a2)

-- Exercicio 7
data Figura = Circulo Ponto Double 
			| Rectangulo Ponto Ponto 
			| Triangulo Ponto Ponto Ponto
			   deriving (Show,Eq)

-- a) 
poligono :: Figura -> Bool
poligono (Circulo c r)= r > 0
poligono (Rectangulo p1 p2) = ((posx p1)/=(posx p2)) && ((posy p1)/=(posy p2))
poligono (Triangulo p1 p2 p3)= let d1= dist6 p1 p2
                                   d2= dist6 p1 p3
                                   d3= dist6 p3 p2
                                in d1+d2>d3 && d2+d3>d1 && d3+d1>d2

-- b)
vertices:: Figura -> [Ponto]
vertices (Circulo (Cartesiano a b) r) = []
vertices (Rectangulo (Cartesiano a b) (Cartesiano c d)) =
 [(Cartesiano a b),(Cartesiano c d),(Cartesiano a d),(Cartesiano c b)]
vertices (Triangulo (Cartesiano a b) (Cartesiano c d) (Cartesiano e f)) =
 [(Cartesiano a b),(Cartesiano c d),(Cartesiano e f)]


--c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =let a = dist6 p1 p2
                               b = dist6 p2 p3
                               c = dist6 p3 p1
                               s = (a+b+c) / 2 -- semi-perimetro
                             in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo p1 p2) = let l= abs (posx p1 - posx p2)
                              a= abs (posy p1 - posy p2)
                          in l*a
area (Circulo p r) = pi*(r^2)


--d)
perimetro' ::Figura -> Double
perimetro' (Circulo (Cartesiano a b) r) = 2*pi*r
perimetro' (Rectangulo (Cartesiano a b) (Cartesiano c d)) = 2*(abs(a-c))+2*(abs(b-d))
perimetro' (Triangulo (Cartesiano a b)(Cartesiano c d)(Cartesiano e f))= sqrt(a^2-c^2+b^2-d^2)+sqrt(a^2-e^2+b^2-f^2)+sqrt(c^2-d^2+e^2-f^2)


--Exercicio 8
isLower'::Char -> Bool
isLower' c = (ord c) >= (ord 'a') && (ord c) <= (ord 'z')

isDigit'::Char ->Bool
isDigit' c = (ord c) >= (ord '0') && (ord c) <= (ord '9')

isAlpha':: Char -> Bool
isAlpha' c = (ord c) >= (ord 'A') && (ord c) <= (ord 'z')

toUpper'::Char -> Char
toUpper' c = chr((ord c)-32)

intToDigit'::Int->Char
intToDigit' a = chr(48+a)

digitToInt':: Char -> Int
digitToInt' a = (ord a)-48


