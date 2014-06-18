import Data.List

mcd :: Integer -> Integer -> Integer
mcd 0 b = b
mcd a b = mcd (b `mod` a) a

data Racional = R Integer Integer

instance Show Racional where
	show (R n d) = (show n) ++ (if d==1 then "" else ("/") ++ (show d))
	
instance Eq Racional where
	(R n1 d1) == (R n2 d2) = (n1*d2 == d1*n2)
	
instance Ord Racional where
	(R n1 d1) <= (R n2 d2) = (n1*d2 <= n2*d1)

crearR :: Integer -> Integer -> Racional
crearR n d = R ((signum d * n) `div` m) ((abs d) `div` m)
   where m = mcd (abs n) (abs d)

crearRI :: Integer -> Racional
crearRI n = crearR n 1

ceroR,unoR :: Racional
ceroR = crearRI 0
unoR = crearRI 1

numerador, denominador :: Racional -> Integer
numerador (R n d) = n
denominador (R n d) = d

sumaR,multR :: Racional -> Racional -> Racional
sumaR (R n1 d1) (R n2 d2) = crearR (n1*d2 + n2*d1) (d1*d2)
multR (R n1 d1) (R n2 d2) = crearR (n1*n2) (d1*d2)
	

data Poli = Cte Racional | Var | Suma Poli Poli | Prod Poli Poli

instance Show Poli
   where show (Cte x) = show x
         show (Var) = "x"
         show (Suma p1 p2) = "(" ++ (show p1) ++ ")+(" ++ (show p2) ++ ")"
         show (Prod p1 p2) = "(" ++ (show p1) ++ ")*(" ++ (show p2) ++ ")"

poli1,poli3 :: [Racional]
poli2 :: Poli

poli1 = [crearR 1 9,crearR (-5) 18,crearR (-5) 9, crearR 5 6, unoR]
poli2 = Suma (Cte (crearR 1 3)) (Prod Var (Suma (Cte (crearR 2 5)) (Prod Var Var) ))
poli3 = [unoR,crearRI (-2),crearRI 2,crearRI (-2),unoR]


--Ejercicio 1
--coeficientes :: Poli -> [Racional]


--Ejercicio 2
polinomio :: [Racional] -> Poli
polinomio [R a b] = Cte (R a b)
polinomio a = Suma (Cte (head a)) (Prod Var (polinomio (tail a)))

--Ejercicio 3
--evaluar :: Poli -> Racional -> Racional

--Ejercicio 4
--raicesRacionales :: [Racional] -> [Racional]
