module KW25T3 where

-- | Type of bind:
-- | (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- | Sequentially compose two actions, passing any 
-- | value produced by the first as an argument to the second.
-- 

-- | Usage of bind and return: [1..10] >>= return . sin

-- | return :: a -> m a
-- | Inject a value into the monadic type.
-- | Without return: (\x -> [f x]) (type: (a -> m b))
map' :: (a -> b) -> [a] -> [b]
map' f xs = xs >>= return . f

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = 
    xs >>= \x -> 
        if p x 
            then [x] -- return x?
            else []

-- | Returns all points from width w to height h
rectangle :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a,b)]
rectangle w h = 
    [0..w] >>= \x -> 
        [0..h] >>= \y ->
            [(x, y)]


