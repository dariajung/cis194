-- Sieve of Sunaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
            let del = delete n in
            2:[2*x + 1 | x <- [1..n], not (x `elem` del) ]

delete n = [i + j + 2*i*j | i <- [1..n], j <- [1..n]]
