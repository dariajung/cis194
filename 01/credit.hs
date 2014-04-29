-- Validation algorithm for credit cards

validate :: [Int] -> Int
validate digits =
   let doubled = reverse (everyOther (reverse digits))
   in (sum $ concat $ map digs doubled) `mod` 10

everyOther :: [Int] -> [Int]
everyOther [] = []
everyOther [a] = [a]
everyOther (x:y:zs) = x : y * 2 : everyOther zs

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

