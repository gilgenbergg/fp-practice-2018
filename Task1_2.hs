module Task1_2 where

import Todo(todo)
import Prelude hiding (gcd, pow)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd 0 0 = error ("Both zeros -> incorrect input for searching dividers")
gcd 0 y = y
gcd x 0 = x
gcd x y = if x>y then gcd y (x `mod` y)
                  else gcd x (y `mod` x)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x y = if even y then pow(x*x) (y `div` 2)
                    else (pow (x*x) ((y-1) `div` 2)) * x

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = if x <= 1 then False
                      else bodyIsPrime [2..x-1] x
        where 
        bodyIsPrime [] x = True              
        bodyIsPrime dividers x = if x `mod` (last dividers) /= 0
                                 then bodyIsPrime (init dividers) x
                                 else False
                --1st version reminder                
                --bodyIsPrime (checked ++ [last dividers]) (init dividers) //"slower variant with additional 
                --"checked" list, filled up with dividers. Then null checked for result.
						

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
