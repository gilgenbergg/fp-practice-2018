module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`
instance  Functor FunMonad where
	{-fmap :: 
	   (a -> b) 
	   -> FunMonad a 
	   -> FunMonad b
    -}
	fmap f (FunMonad a) = FunMonad (f . a)

instance Applicative FunMonad where
	pure a = FunMonad (\anything -> a)
	{-
	(<*>) :: FunMonad (a -> b) -> FunMonad a -> FunMonad b
	-}
	(<*>) fmab fma = FunMonad (\anything -> fun fmab anything (fun fma anything))
	--[(+),(*)] <*> [1,2] <*> [3,4]
	--[4,5,5,6,3,4,6,8]

	-- ((+) <*> (+5)) 1
	--7

--doesn`t work for now, in progress
--instance Monad FunMonad where
--	(>>=) monadA monadB = FunMonad (\anything -> fun (monadB (fun monadA anything) anything)
--	return a = pure a
