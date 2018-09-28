module Task1_1 where

import Todo(todo)
import qualified Data.Map as Map

data BinaryOperation = Plus | Minus | Multiplex deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, binop :: BinaryOperation } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
infixl 6 |+| 

(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus
infixl 6 |-| 

(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Multiplex
infixl 7 |*| 

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`

replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
		case expression of 
			(Variable var) -> if var == varName then replacement else expression
			(BinaryTerm lhv rhv binop) -> BinaryTerm (replaceVar varName replacement lhv) (replaceVar varName replacement rhv) binop 
			_ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = todo
