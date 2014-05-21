import Data.Ratio

mcd :: Integer -> Integer -> Integer
mcd a b | a > b = mcd b a
mcd 0 b = b
mcd a b = mcd (mod b a) a

es_coprimo :: Integer -> Integer -> Bool
es_coprimo n m = (mcd n m) == 1

son_coprimos_a :: Integer -> [Integer] -> Bool
son_coprimos_a q [] = True
son_coprimos_a q (x:xs) = if not (es_coprimo q x)
						  then False
						  else son_coprimos_a q xs

dos_a_n :: Integer -> [Integer]
dos_a_n n = [2..n]

es_divisible_por :: Integer -> Integer -> Bool
es_divisible_por x y = veces_divisible_por y x >  0

dos_a_mitad :: Integer -> [Integer]
dos_a_mitad n = dos_a_n (floor (n % 2))

factores :: Integer -> [Integer]
factores n = filter es_primo (filter (es_divisible_por n) (dos_a_mitad n))

es_primo :: Integer -> Bool
es_primo 1 = False
es_primo n = son_coprimos_a n (dos_a_mitad n)

veces_divisible_por :: Integer -> Integer -> Integer
veces_divisible_por 1 n = undefined
veces_divisible_por p n = if (mod n p) == 0
					      then 1 + veces_divisible_por p (floor(n % p))
					      else 0

multiplicidad :: Integer -> Integer -> (Integer, Integer)
multiplicidad n p = (p, veces_divisible_por p n)

factorizar :: Integer -> [(Integer, Integer)]
factorizar n | n<1 = undefined
factorizar n | es_primo n =  map (multiplicidad n) [n]
factorizar n = map (multiplicidad n) (factores n)

