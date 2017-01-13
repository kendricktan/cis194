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

-- Exercise 3 --
--
-- Finds out the frequency of the numbers in accending order
occurence :: [Integer] -> [Int]
occurence k = [length (filter (n ==) k) | n <- [0..9]]

-- Occurence 2 string
o2s :: [Int] -> Int -> String
o2s [] _ = ""
o2s (x:xs) t = if x >= t then "*" ++ o2s xs t else " " ++ o2s xs t

-- Hmm need state
stateo2s :: [Int] -> Int -> String
stateo2s l i = if i > 0 then o2s l i ++ "\n" ++ stateo2s l (i-1) else "==========\n0123456789\n"

histogram :: [Integer] -> String
histogram n = stateo2s fn (maximum fn)
  where fn = occurence n
