module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = reverseHelper $ reverse lst
  where
     reverseHelper :: [a] -> ReverseList a
     reverseHelper [] = REmpty
     reverseHelper (x: xs) = reverseHelper xs :< x

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec = notImplementedYet
    show REmpty = "[]"
    show lst = "[" ++ showH lst ++ "]"
      where
        showH REmpty = ""
        showH (REmpty :< x) = show x
        showH (xs :< x) = showH xs  ++ "," ++ show x
    
instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True 
    (init1 :< last1) == (init2 :< last2) = init1 == init2 && last1 == last2
    (==) _ _ = False  
    
instance Semigroup (ReverseList a) where
  (<>) xs REmpty = xs
  (<>) xs (ys :< y) = (xs <> ys) :< y
  
instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap _ REmpty = REmpty
  fmap f (xs:<x) = fmap f xs :< f x
  
instance Applicative ReverseList where
  pure = (REmpty :<)
  (<*>) REmpty _ = REmpty
--  f (a -> b) -> f a -> f b
  (<*>) (xs :< x) lst = (xs <*> lst) <> fmap x lst
  
instance Monad ReverseList where
  return = pure
  (>>=) REmpty _ = REmpty
  (>>=) (xs :< x) f = (xs >>= f) <> f x