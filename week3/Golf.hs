{-# OPTIONS_GHC -Wall #-}

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
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:s) = if y > x && y > z
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

histogram :: [Integer] -> String
histogram n = unlines (map (o2s fn) [m,m-1..1]) ++ "==========\n0123456789\n"
  where fn = occurence n
        m = maximum fn
