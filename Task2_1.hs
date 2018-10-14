module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)
-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v =   EmptyTree
				  |Leaf Integer v
				  |Node Integer v (TreeMap v) (TreeMap v)
				  deriving(Show, Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree =  EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _         = False
contains (Leaf k v) e	     | (k == e) = True
					         | otherwise = False
contains (Node k v l r) e    | (k == e) = True
                             | (k < e) = contains r e
                             | (k > e) = contains l e

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ EmptyTree 			= error "Search in empty tree"
lookup e (Leaf k v)  		| (e == k) = v
lookup e (Node k _ _ r)		| (e < k) = lookup k r
lookup e (Node k _ l _)		| (e > k) = lookup k l

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Leaf k v
insert (k, v) (Node key value left right)  | (k == key) = error "Key is already taken"
								           | (k < key) = Node key value (insert (k, v) left) right
								           | (k > key) = Node key value left (insert (k,v) right)



-- Построение списка пар из дерева
--listFromTree :: TreeMap v -> [(Integer, v)]
--listFromTree EmptyTree = []
--listFromTree Leaf key value = [(key, value)]
--listFromTree Node key value left right = listFromTree left ++ [(key, value)] ++ listFromTree right


-- Построение дерева из списка пар
--treeFromList :: [(Integer, v)] -> TreeMap v
--treeFromList lst = foldr (insert) EmptyTree lst


-- Удаление элемента по ключу, при удалении узла с поддеревьями, вместо узла встает 
-- первое поддерево, а второе становится поддерревом первого
--remove :: Integer -> TreeMap v -> TreeMap v
--remove k EmptyTree = EmptyTree
--remove k Leaf key value 			 | (k == key) = EmptyTree
--						  			 | otherwise = Leaf key value
--remove k Node key value left right   | (k == key) = treeFromList (listFromTree left) ++ (listFromTree right)
--									 | (k > key) = --Node key value (remove k left) right-- TODO
--									 | (k < key) = --Node key value left (remove k right)-- TODO


-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = todo

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
