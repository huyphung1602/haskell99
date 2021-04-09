-- Question 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists
data Member a = Single a | Multiple (Int, a) deriving (Eq, Show)

pack :: Eq a => [a] -> [[a]]
pack = foldr packFunc []
  where packFunc x [] = [[x]]
        packFunc x (y:ys) = if x == head y then (x:y):ys else [x]:y:ys

encode :: Eq a => [a] -> [Member a]
encode = map mergeDup . pack
  where mergeDup (x:xs) = if length(x:xs) == 1 then Single x else Multiple (length(x:xs), x)