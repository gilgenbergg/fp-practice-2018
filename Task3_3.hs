module Task3_3 where
-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

--всегда возвращает False
f :: a -> Bool
f _ = False

--всегда возвращает True
t :: a -> Bool
t _ = True

newtype PSet a = PSet{ contains :: (a -> Bool) }
--mappend от двух функций внутри PSet-ов-это скорее применение обеих функций к элементу, 
--по факту вышло, что это соответствует объединению множеств (для инициализации пустое множество)
instance Monoid (PSet a) where
    mempty = PSet (f)
    mappend (PSet s1) (PSet s2) = PSet (\a -> s1 a || s2 a)

--можно попытаться описать аналогично другие операции над множествами,
--для каждого instance требуется отдельный тип
newtype PSetAND a = PSetAND{ containsPSetAND :: (a -> Bool) }
newtype PSetAnotB a = PSetAnotB{ containsPSetAnotB :: (a -> Bool) }
newtype PSetBnotA a = PSetBnotA{ containsPSetBnotA :: (a -> Bool) }
newtype PSetSYMX a = PSetSYMX{ containsPSetSYMX :: (a -> Bool) }

--Пересечение (для инициализации выбрано всегда True)
instance Monoid (PSetAND a) where
    mempty = PSetAND (t)
    mappend (PSetAND s1) (PSetAND s2) = PSetAND (\a -> s1 a && s2 a)

--Симметричная разность (для инициализации пустое множество)
instance Monoid (PSetSYMX a) where
    mempty = PSetSYMX (f)
    mappend (PSetSYMX s1) (PSetSYMX s2) = PSetSYMX ((\a -> s1 a && (not (s2 a)) || (not (s1 a) && s2 a)))

--instance Functor (PSet) where
{-
    f :: a -> b
    f1 :: a -> Bool
    f2 :: b -> Bool
    Внутри функтора с применением fmap нужно получить b -> Bool
    При этом f1 и f2 это наши PSet, которые посредством функций должны примениться к элементу поочередно.
    Я не знаю, как сделать такую композицию средствами языка, известными мне на данный момент, так как
    применнеие fmap в лоб дает что-то вроде: f (a -> Bool) -> f(b -> Bool), то есть...
    у меня два вопроса: как одновременно преобразовав a в b где-то запомнить возвращаемое Bool значение
    то есть применив к elem допустим f1 взять a, "отбросив" (?) возвращаемое Bool значение, но преобрзовав
    в b и применив f2 вернуть Bool.. то есть в случае с Monoid сделать какие-то допущения
    в логике можно вроде более не менее, так как нам по факту не важно, что там внутри,
    здесь же нужно больше информации как-то о внутренней структуре что ли, в общем как наложить
    ограничения на что-то внутри, что имеет по факту тип "функция"..

    вообще не знаю, корректно ли как-то композировать функции типо . в рамках данного задания
    fmap f1 (PSet f2) = PSet (f1 . f2) я не нашла, можно ли так делать и как делать это корректно
-}

