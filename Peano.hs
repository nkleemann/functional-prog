{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NoMonomorphismRestriction #-}
    
module Blueprint where
    
import Test.SmallCheck
import Test.SmallCheck.Series
    
data N = Z | S N deriving (Show , Eq)
    
symdiff :: N -> N -> N
symdiff x y = case x of
    Z -> y
    S x' -> case y of
        Z -> x
        S y' -> symdiff x' y'
    
-- | ghci:  smallCheck 10 spec1    
spec1 = \ (x,y) -> symdiff x y == symdiff y x
spec2 = \ (x,y) -> symdiff x (plus x y) == y
    
    
plus :: N -> N -> N
plus x y = case x of
  Z    -> y
  S x' -> S (plus x' y)
    
test :: Bool
test = and
  [ null $ failures 10 1000 $ spec1
  , null $ failures 10 1000 $ spec2   
  ]
      
instance Monad m => Serial m N where series = cons0 Z \/ cons1 S
    
-- | first f failures from t testcases for property p
failures f t p = take f
  $ filter ( \ x -> not $ p x ) 
  $ take t
  $ do d <- [ 0 .. ] ; list d series


  f :: [a] -> Maybe a
  f xs = Just a