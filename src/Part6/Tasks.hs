{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  eye' :: Int -> mx
  zero' :: Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  eye' _ = 1
  zero' _ _ = 0
  
instance Matrix [[Int]] where
  eye' n = [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]
  zero' n m = [[0 | _ <- [1..n]] | _ <- [1..m]]
  
instance Matrix (SparseMatrix Int) where
  eye' n = SparseMatrix n n (fromList [((i, i), 1) | i <- [0..(n - 1)]])
  zero' n m = SparseMatrix n m mempty

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = eye'

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zero'

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
