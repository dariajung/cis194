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

instance (Show a) => Show (Stream a) where
    show (a :< b) = show (take 20 (streamToList (a :< b)))

streamToList :: Stream a -> [a]
streamToList (x :< xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = x :< streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (x :< xs) = fn x :< streamMap fn xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = x :< streamFromSeed fn (fn x)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :< xs) ys = x :< interleaveStreams ys xs

ruler :: Stream Integer
ruler = let a = streamRepeat 1
            b = streamRepeat 2
            c = streamFromSeed (+1) 3
            d = streamRepeat 0
        in interleaveStreams d (interleaveStreams a (interleaveStreams b c))

