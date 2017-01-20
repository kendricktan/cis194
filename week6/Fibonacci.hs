-- Exercise 1 --
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib $ iterate (+1) 0

-- Exercise 2 --
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs1 (tail fibs1)

-- Exercise 3 --
data Stream a = Empty | Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x Empty) = [x]
streamToList (Cons x y) = x : streamToList y

-- Exercise 4 --
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x Empty) = Cons (f x) Empty
streamMap f (Cons x xs)    = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) (streamFromSeed f (f x))

-- Exercise 5 --
nats :: Stream Integer
nats = streamFromSeed (+1) 0


