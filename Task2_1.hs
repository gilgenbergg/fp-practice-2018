module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)
-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v =    EmptyTree
                  | Leaf Integer v
                  | Node Integer v (TreeMap v) (TreeMap v)
                  deriving(Show, Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree =  EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _         = False
contains (Leaf k v) e        | (k == e) = True
                             | otherwise = False
contains (Node k v l r) e    | (k == e) = True
                             | (k < e) = contains r e
                             | (k > e) = contains l e

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ EmptyTree          = error "Emptyness inside an empty tree :("
lookup e (Leaf k v)         | (e == k) = v
lookup e (Node k v l r)     | (e < k) = lookup k r
                            | (e > k) = lookup k l
                            | otherwise = v

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Leaf k v
insert (k, v) (Leaf key value)   | (k == key) = Leaf key v
                                 | (k < key) = Node key value (Leaf k v) EmptyTree
                                 | (k > key) = Node key value EmptyTree (Leaf k v)
insert (k, v) (Node key value left right)  | (k == key) = Node key value left right
                                           | (k < key) = Node key value (insert (k, v) left) right
                                           | (k > key) = Node key value left (insert (k,v) right)


-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr (insert) EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Leaf key value) = [(key, value)]
listFromTree (Node key value left right) = listFromTree left ++ [(key, value)] ++ listFromTree right


-- Удаление элемента по ключу, при удалении узла с поддеревьями, вместо узла встает 
-- первое поддерево, а второе становится поддерревом первого
remove :: Integer -> TreeMap v -> TreeMap v
remove k EmptyTree = EmptyTree
remove k (Leaf key value)            | (k == key) = EmptyTree
                                     | otherwise = Leaf key value
remove k (Node key value left right) | (k == key) = deleteHandler left right
                                     | (k < key) = Node key value (remove k left) right 
                                     | (k > key) = Node key value left (remove k right)
 where 
       deleteHandler EmptyTree EmptyTree = EmptyTree
       deleteHandler EmptyTree t = t
       deleteHandler (Leaf k v) t = Node k v EmptyTree t
       deleteHandler (Node k v l r) t = Node k v l (deleteHandler r t)


-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ EmptyTree                                  = error "Emptyness inside an empty tree :("
nearestLE k (Leaf key value)                           | (k < key) = (key, value)
                                                       | (k >= key) = error "Not found"
nearestLE k (Node key value left right)                | (k < key) = nearestLE k left
                                                       | (k > key) = checkLeft key value k right 
                                                       | (k == key) = tuffWay key value k left right
 where 
   checkLeft prevk prevl k EmptyTree = (prevk, prevl)
   checkLeft _ _ k (Node key value left (Leaf i val))        | (k == i) = (i, val)
                                                             | otherwise = (key, value)
   checkLeft _ _ k (Node key value left (Node i val ll rr))  | (k == i) = (i, val)
                                                             | (k /= i) = nearestLE i (Node i val ll rr)                                                       
                                                             | otherwise = (key, value)
   
   tuffWay prevk prevv k EmptyTree EmptyTree = error "Not found"
   tuffWay prevk prevv k EmptyTree right = checkLeft prevk prevv k right
   tuffWay prevk prevv k left EmptyTree = nearestLE k left
   tuffWay prevk prevv k left right | (k < prevk) = nearestLE k left
                                    | (k > prevk) = checkLeft prevk prevv k right
                                    | (k == prevk) = tuffWay prevk prevv k left right

--Вычисление размера дерева
size :: TreeMap v -> Integer
size EmptyTree = 0
size (Leaf key value) = 1
size (Node key value left right) = size left + 1 + size right

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ EmptyTree = error "Emptyness inside an empty tree :("
kMean k (Leaf key value)                | (k == 0) = (key, value)
                                        | otherwise = error "Just a looonely leaf :("
kMean k (Node key value left right)     | (k < size left) = kMean k left
                                        | (k > size left) = kMean (k - size left - 1) right
                                        | otherwise = (key, value)

-- Поиск k-той порядковой статистики дерева (нерекурсивный способ)
--kMean :: Integer -> TreeMap v -> (Integer, v)
--kMean _ EmptyTree = error "Tree is empty"
--kMean k t = kMean' (fromIntegral k) t 
  --where
    --kMean' k t | k >= l = error "Error"
      --         | k < 0 = error "Error"
    --           | k < l = listFromTree(t) !! k 
  --  l = length(listFromTree(t))