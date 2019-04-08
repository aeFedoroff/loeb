--введение обобщённо-рекурсивной функции и определение жругих функций с её помощью

--поскольку принцип индукции(та же рекурсия на натуральных числах) невозможен без натуральных чисел
--то необходимо ввести этот тип данных

data Nat = O | S Nat deriving Show

class Recursion a where
  ifZero    :: a -> Bool
  prep      :: a -> a
  toInt     :: a -> Integer


instance Recursion Nat where
  ifZero arg = case arg of
                 O -> True
                 _ -> False
  prep arg   = case arg of
                 O -> O
                 (S a) -> a
  toInt arg = case arg of
                    O     -> 0
                    (S a) -> 1+toInt a 

instance Recursion [a] where
  ifZero xl = case xl of
                [] -> True
                _  -> False
  prep xl  = case xl of
               []   -> []
               x:xs -> xs
  toInt xl = case xl of
               [] -> 0
               x:xs -> 1 + toInt xs

instance Recursion Integer where
  ifZero int = case int of
                 0 -> True
                 _ -> False
  prep int   = case int of
                 0    -> 0
                 int' -> int' - 1
  toInt int  = int
                 
--generalRecursionFunction :: (Recursion a,Recursion b,Recursion c) => a -> b -> b -> (b -> b) -> b -> b 
generalRecursionFunction zeroVal coupFunc arg =
  case ifZero(arg) of
    True -> zeroVal
    _    -> coupFunc (prep arg) (generalRecursionFunction zeroVal coupFunc arg)



--len xl = generalRecursionFunction 0 (+1) xl

-- len :: (Recursion b) => [a] -> b -> (b -> b -> b) -> b 
-- len xl zeroVal coupFunc = case ifZero(xl) of
--                             True  -> zeroVal
--                             False -> (+) (((const 1) . head) xl) (len (prep xl))


len_mod zeroVal coupFunc argFunc arg = if ifZero(arg)
                     then zeroVal
                     else (coupFunc (argFunc arg) (len_mod zeroVal coupFunc argFunc (prep arg)))

len_list = len_mod 0 (+) (((const 1) . head)) 
map_list f xl = len_mod [] (:) (f . head) xl
