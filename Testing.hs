module Testing where

-- This sorting function will get tested in two ways
sort' :: Ord (a) => [a] -> [a]
sort' [] = []
sort' (x:xs) = sort' lesser ++ [x] ++ sort' greater
    where
        lesser  = filter (< x) xs 
        greater = filter (>= x) xs

--| Unit tests (Example values)
unitTestsPass :: Bool
unitTestsPass = and 
    [ []         == sort' ([] :: [Int])
    , [0..10]    == sort' ([10, 9..0]) 
    ]

-- Property based testing:
-- Defining parameterized specifications.
prop_elem :: Ord a => a -> [a] -> Bool
prop_elem x xs = elem x (sort' xs) == elem x xs

prop_ordered :: Ord a => [a] -> Bool
prop_ordered xs = ordered (sort' xs)
    where
        ordered (x:y:xs) = x <= y && ordered (y:xs)
        ordered _        = True

prop_length :: Ord a => [a] -> Bool
prop_length xs = length (sort' xs) == length xs

-- Test the properties like this in ghci:
-- check (prop_elem :: Int -> [Int] -> Bool)
-- +++ OK, passed 200 tests.

-- Using Boolean ==> Implication operator
-- prop_insertOrd x xs = ordered xs ==> ordered (insert x xs)

