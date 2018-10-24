module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (h : t) = foldl f (f acc h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc 
foldr f acc (h : t) = f h (foldr f acc t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Nothing -> []
    Just (a, b') -> a : unfoldr f b'

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h : t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f list = foldr (\x acc -> (f x) : acc) [] list

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldr (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of 
    (Just a) -> a : acc 
    Nothing -> acc) []


-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = reverse . fst . foldl (\(r,c) s -> ((s !! c) : r, c+1)) ([], 0)


-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot filter lst =foldr (\x acc -> if (filter x) then acc else x : acc) [] lst


-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e lst = foldr(\x acc -> if acc then True else x == e) False lst

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = foldl(\ acc x -> 
    if (x `mod` step == from `mod` step) then acc ++ [x] 
                                   else acc
    ) [] [from..to-1]

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append first second = foldr(\x acc -> x : acc) second first

--Разбиение на n кусков
mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt n = foldr (\x (xs, xz) -> if fst x <= n then (snd x : xs, xz) 
                                                        else (xs, snd x : xz)) 
                                                        ([], []) 
                                                        . zip[1..]

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups list n = unfoldr (\x -> if null x then Nothing
                                         else Just (mySplitAt n x)) list
