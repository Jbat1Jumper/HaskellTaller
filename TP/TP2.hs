import Data.List
import Data.Ratio

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
coeficientes :: Poli -> [Racional]
coeficientes (Cte x) = [x]
coeficientes Var = ceroR:[unoR] 
coeficientes (Suma pol1 pol2) = sumarPoli (coeficientes pol1) (coeficientes pol2)
coeficientes (Prod pol1 pol2) = multiplicarPoli (coeficientes pol1)  (coeficientes pol2)

multiplicarPorConst :: Racional -> [Racional] -> [Racional]
multiplicarPorConst a p1 = map (a `multR`) p1

multiplicarPoli :: [Racional] -> [Racional] -> [Racional]
multiplicarPoli [] p2 = []
multiplicarPoli (p:p1) p2 = let pPorP2 = multiplicarPorConst p p2
                                xPorP1Porp2 = multiplicarPorX $ multiplicarPoli p1 p2
                      in sumarPoli pPorP2 xPorP1Porp2 

multiplicarPorX :: [Racional] -> [Racional]
multiplicarPorX p = ceroR:p

sumarPoli :: [Racional] -> [Racional] -> [Racional]
sumarPoli [] [] = []
sumarPoli [] b = sumarPoli [ceroR] b
sumarPoli a [] = sumarPoli a [ceroR]
sumarPoli a b = sumaR (head a) (head b) : sumarPoli (tail a) (tail b)

--Ejercicio 2
polinomio :: [Racional] -> Poli
polinomio [R a b] = Cte (R a b)
polinomio a = Suma (Cte (head a)) (Prod Var (polinomio (tail a)))

--Ejercicio 3
evaluar :: Poli -> Racional -> Racional
evaluar a b = evaluarList (coeficientes a) b

evaluarList :: [Racional] -> Racional -> Racional
evaluarList a b | length a ==1 = head a
evaluarList a b = sumaR (multR (last a) (multPorGrado (tail a) b)) (evaluarList (init a) b)

multPorGrado :: [Racional] -> Racional -> Racional
multPorGrado a b | length a > 0 = multR b (multPorGrado (tail a) b)
multPorGrado a b = unoR

--Ejercicio 3 otra opcion
evaluar2 :: Poli -> Racional -> Racional
evaluar2 (Cte r) _ = r
evaluar2 Var x = x
evaluar2 (Suma a b) x = sumaR (evaluar2 a x) (evaluar2 b x)
evaluar2 (Prod a b) x = multR (evaluar2 a x) (evaluar2 b x)

--Ejercicio 3 otra opcion, por ruffini
evaluar3 :: Poli -> Racional -> Racional
evaluar3 p x = ruffini x (head c) (tail c)
	where c = reverse (coeficientes p)

ruffini :: Racional ->  Racional -> [Racional] -> Racional
ruffini d r [] = r
ruffini d r (p:ps) = ruffini d (sumaR (multR r d) p) ps

--Ejercicio 4
--Usando el teorema de la raiz racional
raicesRacionales :: [Racional] -> [Racional]
raicesRacionales [] = []
raicesRacionales a | head a == ceroR = sort ( nub( ceroR : raicesRacionales (tail a))) -- caso Ter. Ind. == 0
raicesRacionales a = sort (nub (evaluarListRaices a (multiplicarPorConst (crearRI (-1)) posRaices)  ++ evaluarListRaices a posRaices )) 
	where posRaices = (posiblesRaices (posiblesRaicesNum a) (posiblesRaicesDen a))


es_divisible_por :: Integer -> Integer -> Bool
es_divisible_por x y = veces_divisible_por y x >  0

factores :: Integer -> [Integer]
factores n | n < 0 = factores (-n)
factores n = filter (es_divisible_por n) [1..n]

veces_divisible_por :: Integer -> Integer -> Integer
veces_divisible_por 1 n = 1
veces_divisible_por p n = if (mod n p) == 0
					      then 1 + veces_divisible_por p (floor(n % p))
					      else 0

mcmDiv :: [Racional] -> Racional
mcmDiv [] = unoR
mcmDiv a = crearR 1 (lcm (denominador (head a)) (denominador (mcmDiv (tail a))))

mcmDivNum :: [Racional] -> Racional
mcmDivNum a = crearR (denominador (mcmDiv a)) 1

multPorMcm :: [Racional] -> [Racional]
multPorMcm a = map (mcmDivNum a `multR`) a

posiblesRaicesNum:: [Racional] -> [Integer]
posiblesRaicesNum a = factores (numerador (head (multPorMcm a)))

posiblesRaicesDen:: [Racional] -> [Integer]
posiblesRaicesDen a = factores (numerador (last (multPorMcm a)))

posiblesRaices:: [Integer] -> [Integer] -> [Racional]
posiblesRaices num den =  nub ( ceroR : [crearR x y | x <- num, y<- den])

evaluarListRaices :: [Racional] -> [Racional] -> [Racional]
evaluarListRaices pol [] = []
evaluarListRaices pol r | evaluarList pol (head r) == ceroR = head r : evaluarListRaices pol (tail r)
evaluarListRaices pol r	=  evaluarListRaices pol (tail r)
