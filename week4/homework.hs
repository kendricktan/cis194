import Data.List

-- Exercise 1 --
fun1 :: [Integer] -> Integer
fun1 = product . map (2 -) . filter even

fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (>1) $ iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n

-- Exercise 2 --
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

-- Finds the number of subtrees
subtrees :: Tree a -> Integer
subtrees Leaf = 0
subtrees (Node n a b c) = 1 + subtrees a + subtrees c

tinsert :: Tree a -> a -> Tree a
tinsert Leaf              a = Node 0 Leaf a Leaf
tinsert (Node n Leaf c d) a = Node n (Node (n+1) Leaf a Leaf) c d
tinsert (Node n b c Leaf) a = Node n b c (Node (n+1) Leaf a Leaf)
tinsert (Node n b c d)    a
  | subtrees b < subtrees d = Node n (tinsert b a) c d -- Only insert into the smaller sub tree
  | otherwise = Node n b c (tinsert d a)

foldTree :: [a] -> Tree a
foldTree = foldl tinsert Leaf

-- Exercise 3 --
-- Returns True if there are an odd number of True values
xor :: [Bool] -> Bool
xor = foldl (\x y -> not x) False . filter (True==)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f x = foldr (\y ys -> f y : ys) [] x

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f = foldr (flip f) 

-- Exercise 4 --
-- Finds prime numbers using Sieve of Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = tail . map ((+1) . (*2)) $ foldr (\x y -> if x `elem` sn then y else x:y) [] [0..n]
    where sn = takeWhile (((2*n)+2)>=) $ map head . group $ sort [i + j + (2 * i * j) | i <- [1..n], j <- [1..n]]
