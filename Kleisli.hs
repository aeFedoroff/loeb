import Prelude hiding ((.),(>>),(*>))
--посколкьку одним из формальных способов описания рекурсии является композиция
--f(f(f(f(f(f(f..f(0))))))) = f . f . f . ... .f 0
--то стоит определить различные способы композиции функций
--в том числе, в категории Клейсли

--композиция функций имеет следующее стандартное определение применения функций с право на лево
-- (.)   :: (b -> c) -> (a -> b) -> (a -> c) 
-- f . g = \x -> f $ g x 

-- --переопределим композицию в обратном порядке применения функций с лева на право
-- (>>)   :: (a -> b) -> (b -> c) -> (a -> c)
-- f >> g = \x -> g $ f x

--обощение композиции функции с помощью класса Cathegory

data Nat = O | S Nat
  deriving Show

class Category cat where
  id   :: cat a a
  (>>) :: cat a b -> cat b c -> cat a c

--метод id обладает свойством id >> f = f >> id = f
--метод >> ассоциативен (f >> g) >> h = f >> (g >> h)

--класс Клейсли для контейнерных функций

class Kleisli m where
  idK  :: a -> m a
  (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

class Recursion a where
  ifZero :: a -> Bool
  prep   :: a -> Maybe a

instance Recursion [a] where
  ifZero = \xl -> case xl of
                  [] -> True
                  _  -> False
  prep   = \xl -> case xl of
                   (x:xs) -> Just xs
                   []     -> Nothing

instance Recursion Nat where
  ifZero = \nat -> case nat of
                    O -> True
                    _ -> False
  prep   = \nat -> case nat of
                    O      -> Nothing
                    S nat' -> Just nat'  

--экземпляр для функций
instance Category (->) where
  id     = \x -> x
  f >> g = \x -> g $ f x

  
instance Kleisli Maybe where
  idK x  = Just x
  f *> g = \x -> case f x of
                  Nothing -> Nothing
                  Just x' -> g x'
                  
--для клейсли также выполняются свойства f *> idK = idK *> f = f
-- f *> (g *> h) = (f *> g) *> h
--класс Клейсли является классом Категория для специальных функций

--предположим, имеются две функции f:: a -> b и g+:: b -> m c
--композицию вида f >> g+ уже определена, а композицию g+ +> f ещё нужно определить

(+>)   :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
--f +> g = \x -> f *> (g >> idK) $ x
f +> g = f *> (g >> idK)
