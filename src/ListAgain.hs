import System.Random (randomRIO)
import Control.Monad (replicateM)

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

-- Question 23: Extract a given number of randomly selected elements from a list.
-- λ> rndSelect "abcdefgh" 3 >>= putStrLn
-- eda

rndSelect :: Num a => [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect (x:xs) n =
    do r <- randomRIO (0, length xs)
       if r < n
         then do
           rest <- rndSelect xs (n-1)
           return (x : rest)
         else rndSelect xs n


rndSelect' :: [a] -> Int -> IO [a]
rndSelect' xs n 
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, length xs - 1)
                        return (xs!!r)

-- Question 24: Lotto: Draw N different random numbers from the set 1..M.
-- λ> diffSelect 6 49
-- [23,1,17,33,21,37]
diffSelect :: Int -> Int -> IO [Int]
diffSelect n a = diffSelect' n [1..a]

diffSelect':: Int -> [a] -> IO [a]
diffSelect' 0 _ = return []
diffSelect' _ [] = error "No number to choose from"
diffSelect' n xs = do r <- randomRIO (0, length xs - 1)
                      let remainList = take r xs ++ drop (r+1) xs
                      restRndNums <- diffSelect' (n-1) remainList
                      return ((xs!!r):restRndNums)
