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
gcd x y = if x > y then gcd y (x `mod` y)
                  else gcd x (y `mod` x)


squares = map (\x -> x * x) [1..]
-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = squareCheck (reverse[from..to-1])
        where squareCheck elements = if length elements /= 0 then 
                                             if elem (head elements) (takeWhile(<= to-1) squares)
                                                then True
                                                else squareCheck (tail elements)
                                        else False

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = checkDate day month year
        where checkDate day month year =                
                  if day <= 0 || month <= 0 || month >= 13 || year < 0 
                         then False
                  else if day <= (if month `elem` [4,6,9,11] then 30
                                   else if month == 2 then
                                                if (year `mod` 400 == 0) || 
                                                    (year `mod` 100 /= 0) && 
                                                    (year `mod` 4 == 0) 
                                                    then 29
                                               else 28
                                        else 31) 
                        then True
                        else False             
                

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x y = if even y then pow(x * x) (y `div` 2)
                    else (pow (x * x) ((y-1) `div` 2)) * x

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = if x <= 1 then False
                      else bodyIsPrime (reverse[2..x-1]) x
        where 
        bodyIsPrime [] x = True              
        bodyIsPrime dividers x = if x `mod` (head dividers) /= 0
                                 then bodyIsPrime (tail dividers) x
                                 else False

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
