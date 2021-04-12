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
