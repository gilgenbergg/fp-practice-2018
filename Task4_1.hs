module Task4_1 where

import Control.Monad
-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }
-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Monad FunMonad where
	--(>>=) :: FunMonad a -> (a -> FunMonad b) -> FunMonad b
	(>>=) (FunMonad f1) f2 = FunMonad (\anything -> fun (f2 (f1 anything)) anything)
	return a = FunMonad (\anything -> a)

instance  Functor FunMonad where
	{-fmap :: 
	   (a -> b) 
	   -> FunMonad a 
	   -> FunMonad b
    -}
	--fmap f (FunMonad a) = FunMonad (f . a)
	fmap = liftM

instance Applicative FunMonad where
	{-
	(<*>) :: FunMonad (a -> b) -> FunMonad a -> FunMonad b
	-}
	--(<*>) fmab fma = FunMonad (\anything -> fun fmab anything (fun fma anything))
	--[(+),(*)] <*> [1,2] <*> [3,4]
	--[4,5,5,6,3,4,6,8]

	-- ((+) <*> (+5)) 1
	--7

	--(<*>) (FunMonad f1) (FunMonad f2) = FunMonad(\anything -> f1 anything (f2 anything))
	pure = return
	(<*>) = ap
	


