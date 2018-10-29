module Task3_2 where

import Todo(todo)

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

data ReverseList a = RNil | RCons (ReverseList a) a

{- ---head-tail thinking*---
    data ReverseList a = RNil | RCons { head :: ReverseList a, tail :: a}
-}

rlistToList :: ReverseList a -> [a]
rlist RNil = []
rlistToList (RCons t h) = h : (rlistToList t)

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList (h : t) = RCons (listToRList t) h


instance (Show a) => Show (ReverseList a) where
    show RNil = "RNil"
    show (RCons RNil h) = show h ++ " RNil"
    show (RCons h t) = "(RCons " ++ show h ++ show t ++ ")"

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _ = False
    (==) _ RNil = False
    (==) (RCons t1 h1) (RCons t2 h2) = (t1 == t2) && (h1 == h2) 

-----------------------------------------------------------------------------------------
sizeRL :: ReverseList a -> Int
sizeRL (RNil) = 0
sizeRL (RCons t h) = (sizeRL t) + 1


instance (Ord a) => Ord (ReverseList a) where
    compare RNil RNil = EQ
    compare RNil _ = LT 
    compare _ RNil = GT 
    compare (RCons t1 h1) (RCons t2 h2) = 
        if (sizeRL (RCons t1 h1) > sizeRL (RCons t2 h2)) then GT
            else if (sizeRL (RCons t1 h1) < sizeRL (RCons t2 h2)) then LT 
                    else if ((sizeRL (RCons t1 h1) == sizeRL (RCons t2 h2)) && (h1 <= h2)) then LT 
                        else (compare t1 t2) 
---------------------------------------------------------------------------------------------------
concatR :: ReverseList a -> ReverseList a -> ReverseList a
concatR RNil rl = rl
concatR rl RNil = rl
concatR rl (RCons t h) = RCons (concatR rl t) h

instance Semigroup (ReverseList a) where
    (<>) rl2 rl1 = concatR rl2 rl1

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend rl RNil = rl
    mappend RNil rl = rl
    mappend rl (RCons t h) = RCons (mappend rl t) h

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons t h) = RCons (fmap f t) (f h) 