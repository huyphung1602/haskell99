-- Question 1: Find the last element of a list.
-- pattern matching
myLast :: [a] -> a
myLast [] = error "No last for an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- foldl1
myLast' :: [a] -> a
myLast' = foldl1 (\_ x -> x)

-- Question 2: Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [] = error "No last but one element for an empty list"
myButLast [x] = error "No last but one element for a singleton list"
myButLast[x, _] = x
myButLast (_:xs) = myButLast xs

-- Use foldl
myButLast' :: Foldable f => f a -> a
myButLast' = fst . foldl (\(a, b) x -> (b, x)) (err1, err2)
  where err1 = error "No last but one element for an empty list"
        err2 = error "No last but one element for a singleton list"

-- Use Maybe
myButLast'' :: Foldable f => f a -> Maybe a
myButLast'' = fst . foldl (\(a, b) x -> (b, Just x)) (Nothing, Nothing)

-- Question 3: Find the K'th element of a list. The first element in the list is number 1.
-- λ> elementAt [1,2,3] 2
-- 2
-- λ> elementAt "haskell" 5
-- 'e'

-- elementAt :: (Foldable f, Num k, Eq k, Num a) => f a -> k -> Maybe a
elementAt :: (Foldable f, Eq k, Num k) => f a -> k -> Maybe a
elementAt xs k = snd . foldl (\(a, b) x -> if a == k then (a + 1, Just x) else (a + 1, b)) (1, Nothing) $ xs

--  Find the number of elements of a list.
myLength :: Num a => [a] -> a
myLength = foldl (\a _ -> a + 1) 0

myLength' :: Num a => [a] -> a
myLength' [] = 0
myLength' (_:xs) = 1 + myLength' xs
