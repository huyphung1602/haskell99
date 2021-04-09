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

-- Question 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [Member a] -> [a]
decodeModified = concatMap unwrap
  where unwrap (Single a) = [a]
        unwrap (Multiple (k, a)) = replicate k a

-- Question 13:  Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
encode' :: Eq a => [a] -> [Member a]
encode' = foldr infuse []
  where infuse a [] = [Single a]
        infuse b ((Single a):xs) = if a == b then Multiple (2, a): xs else Single b: Single a: xs
        infuse b ((Multiple (k, a)):xs) = if a == b then Multiple (k + 1, a): xs else Single b: Multiple (k, a):xs
