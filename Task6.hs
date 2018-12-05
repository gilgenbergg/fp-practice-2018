module Task6 where

import Todo(todo)

data LinkedTree a = EmptyTree
                  | Leaf a (LinkedTree a)
                  | Node a (LinkedTree a) (LinkedTree a) (LinkedTree a)

instance (Show a) => Show (LinkedTree a) where
    show (EmptyTree _) = "Nil"
    show (Leaf a _) = "Leaf[" ++ show a ++ "]"
    show (Node a l r) = "[l=" ++ show l ++ " Node[" ++ show a ++ "]" ++ "r=" ++show r ++ "]"

instance (Eq a) => Eq (LinkedTree a) where
    (==) (EmptyTree _) (EmptyTree _) = True
    (==) _ (EmptyTree _) = False
    (==) (EmptyTree _) _ = False
    (==) (Leaf a _) (Leaf b _) = a == b
    (==) (Node a la ra _) (Node b lb lr _) = (a == b) && (la == lb) && (ra == lr)
    (==) _ _ = False

emptyTree :: LinkedTree a
emptyTree = EmptyTree emptyTree

find :: LinkedTree a -> a -> Bool
find EmptyTree _             = False
find (Leaf v _) e            | (v == e) = True
                             | otherwise = False
find (Node v l r _) e        | (v == e) = True
                             | (v < e) = find r e
                             | (v > e) = find l e

-- insert :: LinkedTree a -> a -> LinkedTree a
-- insert EmptyTree EmptyTree = Leaf v EmptyTree
-- insert (Leaf a EmptyTree) v         | ()

insert :: LinkedTree a -> a -> LinkedTree a
insert = todo

remove :: LinkedTree a -> a -> LinkedTree a
remove = todo
