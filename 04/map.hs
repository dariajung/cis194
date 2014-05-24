map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
