module KW25T4 where

-- | Monadic functions can be composed with the Kleisli composition operators.
-- | Type: (t -> m a) .. Wrap type t into monadic type (Constraint of bind)
-- | Type: (a -> m b) .. Take an element from monadic list and wrap it into 
--                      another one with diff. type
-- | Type: t          .. Take an input of type t to do the composition on
-- | Type: m b        .. Output is type b wrapped in a monad
(>=>) :: Monad m => (t -> m a) -> (a -> m b) -> t -> m b
f >=> g = \xs -> f xs >>= g

-- | Negate every square number from from 0 to *t*op
neg_sqs t = (map (^2) >=> (return . negate)) [1..t]

-- | commutative?       No  -> Type of arguments differ
-- | Neutral Element?   Yes -> For f : (\xs -> xs) , identity
-- |                              g : (\x  -> [x]), monadic constructor (a -> m a)
-- 

-- | return :: a -> m a
-- | Inject a value into the monadic type.

-- | Version without return:
-- | neg_sqs t = ((map (^2)) >=> (\y -> [negate y])) [1..t]