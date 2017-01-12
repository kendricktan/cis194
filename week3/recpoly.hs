data IntList = Empty | Cons Int IntList
  deriving Show

mapIntList :: (a -> b) -> [a] -> [b]
mapIntList _ Empty      = Empty
mapIntList f (Cons a l) = Cons (f a) (mapIntList f l)

data List t = E | C t (List t)

