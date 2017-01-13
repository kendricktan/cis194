module Golf where

-- Exercise 1 --
--
-- locateIndexes :: [a] -> Index
-- Gets every n element from the list
locateIndexes :: [a] -> Int -> [a]
locateIndexes a n = [a !! x | x <- [(n-1), (n-1+n)..(length a - 1)]]

skips :: [a] -> [[a]]
skips a = [locateIndexes a n | n <- [1..(length a)]]

-- Exercise 2 --
--
-- Is the middle number the largest
isMaxima :: Integer -> Integer -> Integer -> Bool
isMaxima x y z = y > x && y > z

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:s) = if isMaxima x y z
                          then y : localMaxima (y : z : s)
                          else localMaxima (y : z : s)
localMaxima _ = []
