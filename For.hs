module Tricks 
(
    for,
) where

-- | Who said you can't have (((((loops))))) in Haskell?

-- | Iterate over a list
for = flip map

-- | Example
unitMatrix dim = 
    for [1..dim] $ \i ->
        for [1..dim] $ \j ->
            if i == j 
                then 1 
                else 0