{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Blueprint where
import Prelude hiding (dropWhile, takeWhile)

import Test.SmallCheck
import Test.SmallCheck.Series

data List a = Nil | Cons a (List a) deriving (Show, Eq)

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p xs = case xs of
   Nil -> Nil
   Cons x xs' -> case p x of
       False -> Nil
       True  -> Cons x (takeWhile p xs')

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p xs = case xs of
  Nil -> Nil
  Cons x xs' -> case p x of
        True  -> (dropWhile p xs')
        False -> xs

append :: List a ->   List a -> List a
append xs ys = case xs of
  Nil        -> ys
  Cons x xs' -> Cons x (append xs' ys)

spec1 = \ (p, xs :: List Bool ) -> xs == append (takeWhile p xs) (dropWhile p xs)

test :: Bool
test = and [ null $ failures 10 1000 $ spec1 ]

instance (Monad m, Serial m a) => Serial m (List a) where series = cons0 Nil \/ cons2 Cons

-- | first f failures from t testcases for property p
failures f t p = take f
               $ filter ( \ x -> not $ p x )
               $ take t
               $ do d <- [ 0 .. ] ; list d series


l :: List Integer = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 Nil)))))))


