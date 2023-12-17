module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable var)
  | varName == var = replacement
  | otherwise = Variable var
replaceVar varName replacement (BinaryTerm op l r) = BinaryTerm op (replaceVar varName replacement l) (replaceVar varName replacement r)
replaceVar _ _ expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate e = unpackMaybe e $ evaluateInt e

evaluateInt :: Term -> Maybe Int
evaluateInt (IntConstant a) = Just a
evaluateInt (BinaryTerm Plus a b) = maybeOp (evaluateInt a) (evaluateInt b) (+)
evaluateInt (BinaryTerm Minus a b) = maybeOp (evaluateInt a) (evaluateInt b) (-)
evaluateInt (BinaryTerm Times a b) = maybeOp (evaluateInt a) (evaluateInt b) (*)
evaluateInt (Variable _) = Nothing

unpackMaybe :: Term -> Maybe Int -> Term
unpackMaybe _ (Just i) = IntConstant i
unpackMaybe expr Nothing = expr

maybeOp :: Maybe Int -> Maybe Int -> (Int -> Int -> Int) -> Maybe Int
maybeOp (Just a) (Just b) f = Just $ f a b
maybeOp _ _ _ = Nothing