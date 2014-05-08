import Data.Maybe

vacia :: [a] -> Bool
vacia xs = null xs

sumar :: [Int] -> Int
sumar [] = 0
sumar (x:xs) = x + (sumar xs)

multiplicar :: [Int] -> Int
multiplicar [] = 1
multiplicar (x:xs) = x * (multiplicar xs)

concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x:xs) = x ++ (concatenar xs)

maximo :: [Int] -> Int
maximo (x:[]) = x
maximo (x:xs) = if x > y 
				then x 
				else y
			where y = (maximo xs) 
maximo [] = undefined


positivos :: [Int] -> Bool
positivos (x:xs) = (x > 0) && (positivos xs)
positivos [] = True

pares :: [a] -> [b] -> [(a, b)]
pares [] _ = []
pares _ [] = []
pares (x:xs) (y:ys) = (x, y) : (pares xs ys)

suma :: [Int] -> [Int] -> [Int]
suma [] _ = []
suma _ [] = []
suma (x:xs) (y:ys) = (x + y) : (suma xs ys)

mayores :: [Int] -> [Int] -> [Int]
mayores a b | (length a) /= (length b) = undefined
mayores [] [] = []
mayores (x:xs) (y:ys) = (max x y) : (mayores xs ys)

primeros :: Int -> [Int] -> [Int]
primeros l _ | l <= 0 = []
primeros _ [] = []
primeros l (x:xs) = x : (primeros (l - 1) xs)

repetir :: Int -> a -> [a]
repetir l _ | l <= 0 = []
repetir l x = x : (repetir (l -1) x)

creciente :: [Int] -> Bool
creciente [] = True
creciente (x:[]) = True
creciente (x:xs) = (x < (head xs)) && (creciente xs)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = if a == x 
				     then True
				     else (pertenece a xs)

uniq :: [Int] -> [Int]
uniq [] = []
uniq (x:xs) = if pertenece x xs
			  then uniq xs
			  else x : (uniq xs)

sinRepeticiones :: [Int] -> Bool
sinRepeticiones [] = True
sinRepeticiones (x:xs) = if pertenece x xs
						 then False
						 else sinRepeticiones xs
