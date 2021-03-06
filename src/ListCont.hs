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

-- Question 14: Duplicate the elements of a list.
-- dupli [1,2,3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x: dupli xs

-- Question 15: Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli xs 1 = xs
repli (x:xs) k = replicate k x ++ repli xs k

repli' :: [a] -> Int -> [a]
repli' xs k = concatMap (replicate k) xs

-- Question 16: Drop every N'th element from a list.
dropEvery :: (Eq k, Num k) => [a] -> k -> [a]
dropEvery xs k = snd . foldl dropFunc (1, []) $ xs
  where dropFunc (a, b) x
          | a == k = (1, b)
          | otherwise = (a + 1, b ++ [x])

-- Question 17: Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split xs k = foldl distribute ([],[]) xs
  where distribute (a, b) x
          | length a == k = (a, b ++ [x])
          | otherwise = (a ++ [x], b)

-- Question 18: (**) Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
-- ??> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice xs k j = reverse . snd . foldl appendFromTo (1, []) $ xs
  where appendFromTo (a, b) x
          | a < k = (a+1, b)
          | a > j = (a+1, b)
          | otherwise  = (a+1, x:b)

-- Question 19: Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-- ??> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- ??> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate xs k 
  | k >= 0 = slice xs (k + 1) (length xs) ++ slice xs 1 k
  | otherwise = slice xs 1 (length xs - abs k) ++ slice xs (length xs - abs k + 1) (length xs)

-- Question 20: Remove the K'th element from a list.
-- ??> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> ([a], [a])
removeAt k xs = (removedElem, remainList)
  where removedElem = slice xs k k
        remainList = slice xs 1 (k-1) ++ slice xs (k+1) (length xs)

removeAt' :: Int -> [a] -> ([a], [a])
removeAt' k = snd . foldl distribute (1, ([], []))
  where distribute (acc, (a, b)) x
          | acc == k = (acc + 1, (a ++ [x], b))
          | otherwise = (acc + 1, (a, b ++ [x]))
