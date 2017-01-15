import qualified Data.Map as M

main = do
    let emptyMap = M.empty
        mapWithKeys = M.insert 5 "Four" emptyMap
        mapWithKeys' = M.insert 5 "Five" mapWithKeys
    putStrLn $ mapWithKeys' M.! 5
