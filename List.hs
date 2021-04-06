
-- Question 1: last
-- pattern matching
myLast :: [a] -> a
myLast [] = error "No last for an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- foldl1
myLast' :: [a] -> a
myLast' = foldl1 (\_ x -> x)

-- Question 2: My last but one element

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
