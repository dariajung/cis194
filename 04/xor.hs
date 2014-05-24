xor :: [Bool] -> Bool
xor = odd . foldl (\acc x -> if x then acc + 1 else acc) 0
       
 

