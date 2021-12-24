module Part5.Tasks where

import Util (notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ init [] = init
myFoldl f init (h : t) = myFoldl f (f init h) t

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ init [] = init
myFoldr f init (h : t) = f h (myFoldr f init t)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\e t -> f e : t) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr ((++) . f) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (\acc e -> e : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\e acc -> if p e then e : acc else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p list = (myFilter p list, myFilter (not . p) list)
