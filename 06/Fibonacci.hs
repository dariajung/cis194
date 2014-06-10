fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map memoized_fib [0..]

memoized_fib :: Int -> Integer
memoized_fib =
    let fib 0 = 0
        fib 1 = 1
        fib n = memoized_fib (n - 2) + memoized_fib (n - 1)
    in  (map fib [0 ..] !!)

data Stream a = a :< (Stream a)
-- Type  -- Constructor -- Type
--data Stream a = Stream a (Stream a)

instance (Show a) => Show (Stream a) where
    show (a :< b) = show a ++ show (b)

streamToList :: Stream a -> [a]
streamToList (x :< xs) = x : streamToList xs

