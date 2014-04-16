import Data.Ratio

comp :: (Int -> Int) -> (Int -> Int) -> Int -> Int
comp g h x = g ( h (x) )

suma :: Int -> Int -> Int
suma x y = x + y

resta :: Int -> Int -> Int
resta x y = x - y

inc :: Int -> Int
inc x = suma 1 x

mult :: Int -> Int -> Int
mult x y = x * y 


ej2 :: Integer -> Integer -> Rational
ej2 a b = a % b
