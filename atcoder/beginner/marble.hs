main = do
    line <- getLine
    putStrLn $ solve line

solve :: String -> String
solve = show . length . filter ('1'==)