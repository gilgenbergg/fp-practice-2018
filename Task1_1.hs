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
(|+|) lhv rhv = BinaryTerm lhv rhv Plus
infixl 6 |+| 

(|-|) :: Term -> Term -> Term
(|-|) lhv rhv = BinaryTerm lhv rhv Minus
infixl 6 |-| 

(|*|) :: Term -> Term -> Term
(|*|) lhv rhv = BinaryTerm lhv rhv Multiplex
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
evaluate expression = case expression of
	BinaryTerm lhv rhv binop -> eval lhv rhv binop where
		eval lhv rhv binop = case (lhv, rhv) of 
			(IntConstant lhv, IntConstant rhv) -> case binop of
				Plus -> IntConstant (lhv + rhv)
				Minus -> IntConstant (lhv - rhv) 
				Multiplex -> IntConstant (lhv * rhv)
			(lhv, IntConstant 0) -> case binop of
				Plus -> lhv
				Minus -> lhv
				Multiplex -> IntConstant 0
			(IntConstant 0, rhv) -> case binop of 
				Plus -> rhv
				Multiplex -> IntConstant 0
				_ -> BinaryTerm lhv rhv binop
			(IntConstant 1, rhv) -> case binop of 
				Multiplex -> rhv
				_ -> BinaryTerm lhv rhv binop
			(lhv, IntConstant 1) -> case binop of
				Multiplex -> lhv
				_ -> BinaryTerm lhv rhv binop
			_ -> BinaryTerm lhv rhv binop
	_ -> expression	

