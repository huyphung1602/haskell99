-- Question 21: Insert an element at a given position into a list.
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt _ xs 0 = xs
insertAt _ [] _ = []
insertAt x xs k = fst . foldr appendAt ([], length xs) $ xs
  where appendAt y (acc, cnt)
          | k == cnt + 1 && k > length xs = (y:x:acc, cnt - 1)
          | cnt == k = (x:y:acc, cnt - 1)
          | otherwise = (y:acc, cnt - 1)

insertAt' :: a -> [a] -> Int -> [a]
insertAt' _ xs 0 = xs
insertAt' _ [] _ = []
insertAt' x xs k = fst . foldl leftOrRight ([x], 1) $ xs
  where leftOrRight (acc, cnt) y
          | cnt < k = (y:acc, cnt+1)
          | otherwise = (acc ++ [y], cnt+1)

-- Question 22: Create a list containing all integers within a given range.
-- λ> range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range x y = fst . foldr descreaseYtoX ([], 0) $ trueSizeList
  where trueSizeList = replicate (y-x+1) y
        descreaseYtoX num (arr, cnt) = ((num-cnt):arr, cnt + 1)
