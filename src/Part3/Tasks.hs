module Part3.Tasks where

import Data.List
import Util (notImplementedYet)
import Data.Ord (comparing)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq numbers = head $ maximumBy (comparing length) $ group $ sort $ concatMap numberToDigit numbers
  where
    numberToDigit :: Int -> [Int]
    numberToDigit a
      | abs a < 10 = [abs a]
      | otherwise = mod a 10 : numberToDigit (div a 10)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = map (\lst -> (fst $ head lst, map snd lst)) $ groupBy (\a b -> fst a == fst b) $ sortBy (\a b -> if fst a == fst b then EQ else LT) $ map (\x -> (f x, x)) l
