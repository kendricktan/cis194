locateIndexes :: [a] -> Int -> [a]
locateIndexes a n = [a !! x | x <- [(n-1), (n-1+n)..(length a - 1)]]

skips :: [a] -> [[a]]
skips a = [locateIndexes a n | n <- [1..(length a)]]
