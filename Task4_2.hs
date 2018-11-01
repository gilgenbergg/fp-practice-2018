module Task4_2 where
import Control.Monad

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

f1 (FourOf s _ _ _) = s
f2 (FourOf _ s _ _) = s
f3 (FourOf _ _ s _) = s
f4 (FourOf _ _ _ s) = s

instance Monad (FourOf) where
	(>>=) (FourOf a b c d ) f = FourOf (f1 (f a)) (f2 (f b)) (f3 (f c)) (f4 (f d))
	return s = FourOf s s s s

instance Functor FourOf where
	fmap = liftM
	--fmap f (FourOf a b c d ) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
	(<*>) = ap 
	--(<*>) (FourOf a b c d) (FourOf a' b' c' d') = FourOf (a a') (b b') (c c') (d d')
	pure = return 