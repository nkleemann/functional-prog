{-# language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Blueprint where

import Test.SmallCheck
import Test.SmallCheck.Series

data N = Z | S N deriving ( Eq, Show )

fold :: b -> (b -> b) -> N -> b
fold z s n = case n of
    Z    -> z
    S n' -> s (fold z s n')

plus :: N -> N -> N
plus x y = fold y S x

pre :: N -> N
pre = fst . fold (Z, Z) (\(p, q) -> let p' = q in (p', S q))

spec1 = \ x -> pre (S x) == x

minus :: N -> N -> N
minus x y = fold x pre y

spec2 = \ (x, y) -> minus (plus x y) x == y
spec3 = \ (x, y) -> minus x (plus x y) == Z

test = and [ null $ failures 10 1000 spec1
           , null $ failures 10 1000 spec2
           , null $ failures 10 1000 spec3
           ]

instance Monad m => Serial m N where
  series = cons0 Z \/ cons1 S

-- | first f failures from t testcases for property p
failures f t p = take f
           $ filter ( \ x -> not $ p x )
           $ take t
           $ do d <- [ 0 .. ] ; list d series
