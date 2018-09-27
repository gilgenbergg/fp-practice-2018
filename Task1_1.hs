module Task1_1 where

import Todo(todo)
import qualified Data.Map as Map

data BinaryOperation = Plus | Minus | Multiplex deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
infixl 6 |+| Term -> Term
(|-|) l r = BinaryTerm l r Minus
infixl 6 |-| Term -> Term
(|*|) :: Term -> Term -> Term
(|*|) l r BinaryTerm l r Multiplex
infixl 7 |*| Term -> Term

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = todo

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = todo
