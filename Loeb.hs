module Loeb where

loeb   :: Functor f => f (f a -> a) -> f a
loeb x = go
  where
    go = fmap ($ go) x


loebLambda   :: Functor f => f (f a -> a) -> f a
loebLambda x = go
  where
    go = fmap (\y -> y go) x

fs = [ const 1
     ,succ . (!! 0)
     ,succ . (!! 1)
     ,succ . (!! 2)
     ]

loeb fs     
