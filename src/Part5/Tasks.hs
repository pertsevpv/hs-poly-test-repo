module Part5.Tasks where

import Util (notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ e [] = e
myFoldl f e (x : xs) = f (myFoldl f e xs) x

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ e [] = e
myFoldr f e (x : xs) = f x (myFoldr f e xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\y -> (f y :)) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = foldr (\y -> (f y <>)) []

myConcat :: [[a]] -> [a]
myConcat = myConcatMap id

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\x acc -> if p x then x : acc else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\x (accT, accF) -> if p x then (x : accT, accF) else (accT, x: accF)) ([], [])

