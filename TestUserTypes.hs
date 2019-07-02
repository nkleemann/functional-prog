{-|
    Testing User defined data types with
    poperty based testing by LeanCheck.
-}
module TestTypes where

import Test.LeanCheck

data Stack a = Stack a (Stack a) | Empty deriving (Show,Eq)

push :: a -> Stack a -> Stack a
push x s = Stack x s

pop :: Stack a -> (a, Stack a)
pop (Stack x s) = (x, s)

prop_popush :: (Eq a) => a -> Stack a -> Bool
prop_popush x s =  pop (push x s) == (x,s)

-- Into ghci:
-- check (prop_popush :: Int -> Stack Int -> Bool)
-- But we get an error:
--      No instance for (Listable (Stack Int))

-- Stack should be an instance of Listable typeclass

instance Listable a => Listable (Stack a) where
    tiers = cons2 Stack \/ cons0 Empty

-- check (prop_popush :: Int -> Stack Int -> Bool)
-- this works now :)
