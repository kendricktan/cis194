{-# LANGUAGE FlexibleInstances #-}

data Foo = F Int | G Char deriving (Show)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

instance Eq Foo where
    (F i1) == (F i2) = i1 == i2
    (G c1) == (G c2) = c1 == c2
    _ == _ = False

---------------------------------------

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList x = [x]

instance Listable Bool where
    toList True = [1]
    toList False = [0]

instance Listable [Int] where
    toList = id

instance Listable (Tree Int) where
    toList Empty        = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

instance (Listable a, Listable b) => Listable (a, b) where
    toList (x, y) = toList x ++ toList y

sumL x = sum (toList x)

foo x y = sum (toList x) == sum (toList y) || x < y
