module Task3_3 where
-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

--Для использования внутри каждого отдельного моноида нужен отдельный PSet
newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSetOR a = PSetOR{ containsPSetOR :: (a -> Bool) }
newtype PSetAND a = PSetAND{ containsPSetAND :: (a -> Bool) }
newtype PSetAnotB a = PSetAnotB{ containsPSetAnotB :: (a -> Bool) }
newtype PSetBnotA a = PSetBnotA{ containsPSetBnotA :: (a -> Bool) }
newtype PSetSYMX a = PSetSYMX{ containsPSetSYMX :: (a -> Bool) }

{-
    Rак реализовать функтор с учетом отсутствия понимания по типам возвращаемого значения?
    То есть по идее компилятору надо понимать, что возвращается Bool, но у нас внутри по факту не элементы,
    а отображения в виде функций, как это преобразовать, я не знаю.
    По идее fmap работает в любыми типами, но исходя из контекста накладываются ограничения, а как наложить
    эти ограничения здесь, то есть показать, что на вход fmap поступает функция, точно возвращающая Bool, я не понимаю как.
-}

f :: a -> Bool
f _ = False

t :: a -> Bool
t _ = True

--Для PSet мне не ясен из задания принцип обработки, я не знаю, что с ним делать

--instance Monoid PSet a where
    --mempty = 
    --mappend = 

--Объединение
instance Monoid (PSetOR a) where
    mempty = PSetOR (f)
    mappend (PSetOR s1) (PSetOR s2) = PSetOR (\a -> s1 a || s2 a)

--Пересечение
instance Monoid (PSetAND a) where
    mempty = PSetAND (t)
    mappend (PSetAND s1) (PSetAND s2) = PSetAND (\a -> s1 a && s2 a)

--A без B
instance Monoid (PSetAnotB a) where
    mempty = PSetAnotB (f)
    mappend (PSetAnotB s1) (PSetAnotB s2) = PSetAnotB (\a -> s1 a && (not (s2 a)))

--B без A
instance Monoid (PSetBnotA a) where
    mempty = PSetBnotA (f)
    mappend (PSetBnotA s1) (PSetBnotA s2) = PSetBnotA (\a -> (not (s1 a) && s2 a))

--Симметричная разность
instance Monoid (PSetSYMX a) where
    mempty = PSetSYMX (f)
    mappend (PSetSYMX s1) (PSetSYMX s2) = PSetSYMX ((\a -> s1 a && (not (s2 a)) || (not (s1 a) && s2 a)))