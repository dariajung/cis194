-- Validation algorithm for credit cards
 
validate :: Integer -> Bool
validate digits
    | total `mod` 10 == 0   = True
    | otherwise             = False
    where doubled = everyOther (reverse (getDigits digits))
          total = sum $ concat $ map getDigits doubled
 
everyOther :: [Integer] -> [Integer]
everyOther [] = []
everyOther [a] = [a]
everyOther (x:y:zs) = x : y * 2 : everyOther zs
 
getDigits :: Integer -> [Integer]
getDigits 0 = [0]
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]

acceptInput :: String -> Integer
acceptInput x = read (filter (`elem` ['0'..'9']) x) :: Integer



