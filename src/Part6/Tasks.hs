{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  eye' :: Int -> mx
  zero' :: Int -> Int -> mx
  row :: mx -> Int -> [Int]
  col :: mx -> Int -> [Int]
  rows :: mx -> Int
  cols :: mx -> Int
  mFromList :: [[Int]] -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  eye' _ = 1
  zero' _ _ = 0
  row mx _ = [mx]
  col mx _ = [mx]
  rows _ = 1
  cols _ = 1
  mFromList ((x : _) : _) = x

instance Matrix [[Int]] where
  eye' n = [[if i == j then 1 else 0 | j <- [1 .. n]] | i <- [1 .. n]]
  zero' n m = [[0 | _ <- [1 .. n]] | _ <- [1 .. m]]
  row mx i = mx !! i
  col mx i = Prelude.map (!! i) mx
  rows mx = length mx
  cols [] = 0
  cols (x : _) = length x
  mFromList = id

instance Matrix (SparseMatrix Int) where
  eye' n = SparseMatrix n n (fromList [((i, i), 1) | i <- [0 .. (n - 1)]])
  zero' n m = SparseMatrix n m mempty
  row (SparseMatrix _ m mx) i = [unpackMaybe (Data.Map.lookup (i, j) mx) | j <- [0 .. m - 1]]
  col (SparseMatrix n _ mx) i = [unpackMaybe (Data.Map.lookup (j, i) mx) | j <- [0 .. n - 1]]
  rows (SparseMatrix n _ _) = n
  cols (SparseMatrix _ m _) = m
  mFromList lst =
    SparseMatrix
      (rows lst)
      (cols lst)
      (Data.Map.filter (/= 0) (fromList $ concat [[((i, j), (lst !! i) !! j) | j <- [0 .. (cols lst - 1)]] | i <- [0 .. (rows lst - 1)]]))

unpackMaybe :: Maybe Int -> Int
unpackMaybe (Just a) = a
unpackMaybe Nothing = 0

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = eye'

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zero'

-- Перемножение матриц
mul :: [Int] -> [Int] -> Int
mul [] [] = 0
mul (a : as) (b : bs) = a * b + mul as bs
mul _ _ = notImplementedYet

multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b = mFromList [[mul (row a i) (col b j) | j <- [0 .. cols b - 1]] | i <- [0 .. rows a - 1]]

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
