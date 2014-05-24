--xor :: [Bool] -> Bool
-- this gives me an error, not sure why yet
xor = odd $ foldl (\acc x -> if x then acc + 1 else acc) 0
       
 

