module Part1.Tasks where

import Util(notImplementedYet)

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

taylor :: Double -> (Double -> Integer -> Double) -> Double
taylor x f = sum (map (f x) [0..10])

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = taylor x sinTaylor

sinTaylor :: Double -> Integer -> Double
sinTaylor x n = ((-1) ^ n) * myPowD x (2 * n + 1) / fromIntegral (fac (2 * n + 1))

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = taylor x cosTaylor

cosTaylor :: Double -> Integer -> Double
cosTaylor x n = myPowD (-1) n * myPowD x (2 * n) / fromIntegral (fac (2 * n))

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = myAbsGCD (abs a) (abs b)
  
myAbsGCD :: Integer -> Integer -> Integer
myAbsGCD a 0 = a
myAbsGCD a b
  | a < b = myGCD b a
  | otherwise = myGCD b (mod a b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d 1 y = between 0 d 31 && y >= 0
isDateCorrect d 3 y = between 0 d 31 && y >= 0
isDateCorrect d 4 y = between 0 d 30 && y >= 0
isDateCorrect d 5 y = between 0 d 31 && y >= 0
isDateCorrect d 6 y = between 0 d 30 && y >= 0
isDateCorrect d 7 y = between 0 d 31 && y >= 0
isDateCorrect d 8 y = between 0 d 31 && y >= 0
isDateCorrect d 9 y = between 0 d 30 && y >= 0
isDateCorrect d 10 y = between 0 d 31 && y >= 0
isDateCorrect d 11 y = between 0 d 30 && y >= 0
isDateCorrect d 12 y = between 0 d 31 && y >= 0
isDateCorrect d 2 y
  | isLeap = between 0 d 29
  | otherwise = between 0 d 28
  where
    isLeap :: Bool
    isLeap = (mod y 400 == 0) || (mod y 100 /= 0 && mod y 4 == 0)
isDateCorrect _ _ _ = False

between :: Integer -> Integer -> Integer -> Bool
between a x b = a <= x && x <= b

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow x n =
  if even n
    then myPow (x * x) (div n 2)
    else x * myPow x (n - 1)

myPowD :: Double -> Integer -> Double
myPowD _ 0 = 1
myPowD x n
  | even n = myPowD x (div n 2) * myPowD x (div n 2)
  | otherwise = x * myPowD x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = foldr (&&) True (map (\a -> mod x a /= 0) [2 .. (div x 2)])

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea (x : xs) = abs (sum (zipWith (\(x1, y1) (x2, y2) -> x1 * y2 - x2 * y1) (x : xs) (xs ++ [x]))) / 2

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | not $ c < a + b && b < a + c && a < b + c = -1
    | a2 + b2 == c2 || a2 + c2 == b2 || b2 + c2 == a2 = 2
    | a2 + b2 < c2 || a2 + c2 < b2 || b2 + c2 < a2 = 0
    | otherwise = 1
  where 
    a2 = myPowD a 2
    b2 = myPowD b 2
    c2 = myPowD c 2