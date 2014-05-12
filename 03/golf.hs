module Golf where

--skip :: [a] -> [[a]]

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []
