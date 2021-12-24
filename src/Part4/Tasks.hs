{-# LANGUAGE InstanceSigs #-}

module Part4.Tasks where

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

instance Eq a => Eq (ReverseList a) where
  (==) REmpty REmpty = True
  (==) _ REmpty = False
  (==) REmpty _ = False
  (==) (t1 :< h1) (t2 :< h2) = h1 == h2 && (t1 == t2)
  (/=) a b = not (a == b)

instance Semigroup (ReverseList a) where
  -- Union operation
  (<>) :: ReverseList a -> ReverseList a -> ReverseList a
  (<>) e REmpty = e
  (<>) REmpty e = e
  (<>) l1 (t2 :< h2) = addToRlist (l1 <> t2) h2
    where
      addToRlist REmpty elem = REmpty :< elem
      addToRlist (t :< h) elem = t :< h :< elem

instance Monoid (ReverseList a) where
  -- Just Semigroup with 'e'
  mempty = REmpty

instance Functor ReverseList where
  -- Applying function (a -> b) to each element with saving source structure
  fmap :: (a -> b) -> ReverseList a -> ReverseList b
  fmap f REmpty = REmpty
  fmap f (t :< h) = fmap f t :< f h

instance Applicative ReverseList where
  -- Note: pure f<*>x === fmap f x
  pure :: a -> ReverseList a
  pure a = REmpty :< a

  -- Possibility to apply Functor's operator to multiple expressions
  -- Example 1: fmap (*) [1,2,3] <*> [3,4] === [3,4,6,8,9,12]
  -- Example 2: (++) <$> ["ha","heh","hmm"] <*> ["?","!","."] ===
  --              ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
  (<*>) :: ReverseList (a -> b) -> ReverseList a -> ReverseList b
  (<*>) _ REmpty = REmpty
  (<*>) REmpty _ = REmpty
  (<*>) (t1 :< h1) (t2 :< h2) = (t1 <*> (t2 :< h2)) <> (fmap h1 t2 :< h1 h2)

instance Monad ReverseList where
  (>>=) :: ReverseList a -> (a -> ReverseList b) -> ReverseList b
  (>>=) xs k = flatten $ fmap k xs --k =<< xs -- === join $ fmap k xs

flatten :: ReverseList (ReverseList a) -> ReverseList a
flatten REmpty = REmpty
flatten (REmpty :< h) = h
flatten (t :< int) = flatten t <> int