module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = let
	if x == 0 then = y
	if y == 0 then = x
	else if x>y = gcd y (x `mod` y)
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
pow x y = let
	if y == 0 = 1
	if even y then = pow(x*x) (y `div` 2)
	else = (pow (x*x) ((y-1) `div` 2) * x

-- является ли данное число простым?
isPrime :: Integer -> Bool
	isPrime x = if x<=1 
		then False
		else bodyIsPrime
	where bodyIsPrime = let
		dividers = [2..x]
		checked = []
		recursionCheck = let
			if x `mod` (last dividers) <> 0
				then init dividers
					recursionCheck
				else checked ++ [last dividers]
					init dividers
						recursionCheck
			if null checked then True
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
