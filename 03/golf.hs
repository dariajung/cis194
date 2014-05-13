module Golf where

skips :: [a] -> [[a]]
skips xs = map (every xs) [1..(length xs)]

every xs n = case drop (n-1) xs of
              (y:ys) -> y : every ys n 
              [] -> []

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:list@(y:z:zs))
    | x < y && (z) < y     = y : localMaxima list 
    | otherwise             = localMaxima list 
localMaxima (_:x) = []

histogram :: [Integer] -> String
histogram xs =
             let freq@(y:ys) = map (\x -> instances x xs) [0..9]
             in (draw freq ++ "\n") ++ "==========\n0123456789\n"

-- maybe a draw function?
draw [] = ""
draw list@(x:xs)
    | x > 0   = "*" ++ draw xs
    | otherwise  = " " ++ draw xs

instances x [] = 0 
instances x (y:ys) 
    | x == y    = 1 + (instances x ys)
    | otherwise = instances x ys
