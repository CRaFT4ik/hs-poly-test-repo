{-# LANGUAGE MultiWayIf #-}

module Part1.Tasks where

import Data.List (nub, delete)
import Util (notImplementedYet)

fact :: Integer -> Double
fact 0 = 1
fact n = fromIntegral n * fact (n - 1)

factorial n = product [1 .. n]

checkEps :: Double -> Bool
checkEps x = abs x <= 1e-16

inRadians :: Double -> Double
inRadians x = let circle = 2 * pi in x - circle * fromIntegral (floor (x / circle))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinAdd (inRadians x) 0

sinAdd :: Double -> Integer -> Double
sinAdd x k =
  let m = 2 * k + 1
      res = (-1) ^ k * (x ^^ m) / fact m
   in if checkEps res then res else res + sinAdd x (k + 1)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = cosAdd (inRadians x) 0

cosAdd x k =
  let m = 2 * k
      res = (-1) ^ k * (x ^^ m) / fact m
   in if checkEps res then res else res + cosAdd x (k + 1)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD inX inY =
  let x = abs inX
      y = abs inY
   in if
          | x == 0 || y == 0 -> x + y
          | x > y -> myGCD (x `rem` y) y
          | otherwise -> myGCD x (y `rem` x)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y =
  let leap = isLeap y
   in if
          | d < 1 || d > 31 || m < 1 || m > 12 -> False
          | leap && m == 2 && d > 29 -> False
          | not leap && m == 2 && d > 28 -> False
          | odd d && d == 31 -> False
          | otherwise -> True

isLeap :: Integer -> Bool
isLeap year =
  if
      | year `rem` 4 /= 0 -> False
      | year `rem` 100 /= 0 -> True
      | year `rem` 400 /= 0 -> False
      | otherwise -> True

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow a n = myPow' a n 0

myPow' a i sum =
  sum
    + if
        | i == 0 -> 1
        | i == 1 -> a
        | otherwise -> a * myPow' a (i - 1) 0

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime x = and [x `mod` y /= 0 | y <- [2 .. (x -1)]]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = 0.5 * abs (shapeCalc pointsNum 0 uniqPoints)
  where
    uniqPoints = nub points
    pointsNum = length uniqPoints

shapeCalc :: Int -> Int -> [Point2D] -> Double
shapeCalc n i lst =
  if i == (n - 1)
    then
      let (x_n, y_i) = lst !! (n - 1)
          (x_0, y_0) = head lst
       in x_n * y_0 - x_0 * y_i
    else
      let (x_i, y_i) = lst !! i
          (x_j, y_j) = lst !! (i + 1)
       in x_i * y_j - x_j * y_i + shapeCalc n (i + 1) lst

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c =
  if a + b <= c || b + c <= a || a + c <= b then (-1) else
    if | cc == aa + bb -> 2
       | cc < bb + aa -> 1
       | otherwise -> 0
    where max = maximum [a, b, c]
          min = minimum [a, b, c]
          mid = head . delete min . delete max $ [a, b, c]
          cc = max ^^ 2
          aa = min ^^ 2
          bb = mid ^^ 2