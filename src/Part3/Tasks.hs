module Part3.Tasks where

import Data.List (nubBy)
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq list =
  let digits = concatMap getDigits list
      uniqDigits = uniq digits
   in fst . findMostFreq $ map (\e -> (e, length . filter (== e) $ digits)) uniqDigits

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits n = n `mod` 10 : getDigits (n `div` 10)

findMostFreq :: [(Int, Int)] -> (Int, Int)
findMostFreq [] = (0, -1)
findMostFreq ((number, count) : tail) =
  let (nextNumber, nextCount) = findMostFreq tail
   in if count > nextCount then (number, count) else (nextNumber, nextCount)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (h : t) = h : uniq (filter (h /=) t)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l =
  let fls = map (\e -> (e, f e)) l
      uniqFls = nubBy (\x y -> snd x == snd y) fls
   in map (\e -> (snd e, map fst $ filter (\(_, k2) -> k2 == snd e) fls)) uniqFls
