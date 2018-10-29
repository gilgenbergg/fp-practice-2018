module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance  Show WeirdPeanoNumber where
    show wpn = wpnShow wpn

wpnShow :: WeirdPeanoNumber -> String
wpnShow Zero = "0"
wpnShow (Succ x) = "Succ(" ++ show x ++ ")"
wpnShow (Pred x) = "Pred(" ++ show x ++ ")"

----------------For Eq instance------------------
eQ :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
eQ Zero Zero = True
eQ Zero _ = False
eQ _ Zero = False
eQ (Succ a) (Succ b) = eQ a b 
eQ (Pred a) (Pred b) = eQ a b
eQ (Succ a) (Pred b) = False
eQ (Pred a) (Succ b) =  False

------------For =/- compensations----------------
parseWPN :: WeirdPeanoNumber -> WeirdPeanoNumber 
parseWPN Zero = Zero
parseWPN (Pred (Succ a)) = parseWPN a
parseWPN (Succ (Pred a)) = parseWPN a
parseWPN (Pred a) = Pred $ parseWPN a
parseWPN (Succ a) = Succ $ parseWPN a
-------------------------------------------------
instance  Eq WeirdPeanoNumber where
    (==) a b = eQ (parseWPN a) (parseWPN b)

-------------convert WPN to int------------------
wpnToInt :: WeirdPeanoNumber -> Int 
wpnToInt Zero = 0
wpnToInt (Succ x) = wpnToInt x + 1
wpnToInt (Pred x) = wpnToInt x - 1

-------------convert Int to WPN------------------
intToWPN :: Int -> WeirdPeanoNumber
intToWPN a | a == 0 = Zero
           | a < 0 = Pred (intToWPN $ a + 1) 
           | a > 0 = Pred (intToWPN $ a - 1)


instance  Enum WeirdPeanoNumber where
    toEnum = intToWPN
    fromEnum = wpnToInt  

-------------------comparator--------------------
comparator :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
comparator Zero Zero = EQ 
comparator Zero (Succ _) = LT
comparator (Succ _) Zero = GT
comparator Zero (Pred _) = GT
comparator (Pred _) Zero = LT
comparator (Succ a) (Succ b) = comparator a b
comparator (Succ a) (Pred b) = GT
comparator (Pred a) (Succ b) = LT
comparator (Pred a) (Pred b) = comparator a b

-------------------------------------------------
instance Ord WeirdPeanoNumber where
    compare a b = comparator a b 

instance  Num WeirdPeanoNumber where
    negate Zero = Zero
    negate (Succ a) = Pred (negate a)
    negate (Pred a) = Succ (negate a)
    signum Zero = Zero
    signum (Pred (Succ a)) = signum a
    signum (Succ (Pred a)) = signum a
    signum (Pred a) = Pred Zero
    signum (Succ a) = Succ Zero
    abs a = if (signum a < Zero) then negate a else a
    (+) Zero a = a
    (+) a Zero = a
    (+) (Succ a) b = Succ $ a + b  
    (+) (Pred a) b = Pred $ a + b 
    (*) Zero a = Zero
    (*) a Zero = Zero
    (*) 1 a = a 
    (*) a 1 = a
    (*) (-1) a = negate a
    (*) a (-1) = negate a
    (*) (Succ a) b = b + (a * b)
    (*) (Pred a) b = if (signum a == signum b) then (b + (a * b))
                    else if (signum a /= signum b) && (signum a < Zero) then negate (b + ((negate a) * b))
                    else negate ((negate b) + a * (negate b))   
    fromInteger a | a == 0 = Zero
                  | a < 0 = Pred (fromInteger (a + 1))
                  | a > 0 = Succ (fromInteger (a - 1)) 

instance  Real WeirdPeanoNumber where
    toRational a = toRational (wpnToInt a)    

-----------for quotRem implementation------------
--makeDivision :: WeirdPeanoNumber -> WeirdPeanoNumber -> 


instance  Integral WeirdPeanoNumber where 
    toInteger Zero = 0
    toInteger (Succ a) = (toInteger a + 1)
    toInteger (Pred a) = (toInteger a - 1)
    --quotRem a b = makeDivision (parseWPN a) (parseWPN b)