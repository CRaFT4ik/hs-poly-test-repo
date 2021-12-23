{-# LANGUAGE InstanceSigs #-}

module Part4.Tasks where

import Data.List
import Util (notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
  reverse (reversed lst)
  where
    reversed REmpty = []
    reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
  show :: Show a => ReverseList a -> String
  show list = "[" ++ elems list ++ "]"
    where
      elems :: Show a => ReverseList a -> String
      elems REmpty = ""
      elems (t :< h) =
        let others = elems t
         in if others == "" then show h else others ++ "," ++ show h

instance Eq (ReverseList a) where
  (==) = notImplementedYet
  (/=) = notImplementedYet

instance Semigroup (ReverseList a)

instance Monoid (ReverseList a)

instance Functor ReverseList

instance Applicative ReverseList

instance Monad ReverseList
