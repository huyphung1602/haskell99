
myLast :: [a] -> a
-- pattern matching
myLast [] = error "No last for an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- foldl1
-- myLast = foldl1 (\_ x -> x)