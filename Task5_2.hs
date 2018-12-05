module Task5_2 where

import Todo(todo)
import Data.Ratio
import Prelude hiding (scanl, take, zipWith, (%))

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                } deriving Show

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)


generateS :: (Eq a, Num a) => a -> a -> Stream a
generateS 1 0 = Cons (-1) (generateS 0 1)
generateS (-1) 0 = Cons (1) (generateS 0 (-1))
generateS 0 1 = Cons 0 (generateS (-1) 0)
generateS 0 (-1) = Cons 0 (generateS 1 0)

scanl :: (a -> a -> a) -> a -> Stream a -> Stream a
scanl f init (Cons shead stail) = Cons init (scanl f (f init shead) stail)

factorials :: Stream Integer
factorials = scanl (*) 1 (generate 1 (+ 1))

denoms :: Num a => a -> Stream a
denoms x = fmap (x^) (generate 0 (+ 1))

sin' :: Stream Double -> Stream Double
sin' = zipWith (*) (generateS 0 1) 

--основная функция комбинирования
sinPrecisions :: Double -> Stream Double
sinPrecisions x = stail (scanl (+) 0 (sin' (zipWith (/) (denoms x) (fmap fromInteger factorials))))

exp' :: Rational -> Stream Rational
exp' x = zipWith (/) (denoms x) (fmap toRational factorials)

ePrecisions :: Stream Rational
ePrecisions = stail (scanl (+) 0 (exp' 1))

zipWith :: (a -> a -> a) -> Stream a -> Stream a -> Stream a
zipWith fun (Cons shead1 stail1) (Cons shead2 stail2) = Cons (fun shead1 shead2) (zipWith fun stail1 stail2)

take :: Integer -> Stream a -> [a]
take 0 stream = []
take x (Cons shead stail) = shead : (take (x - 1) stail)
