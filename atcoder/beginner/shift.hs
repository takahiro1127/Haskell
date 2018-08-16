main = do
    line <- getLine
    numbers <- fmap (map read . words) getLine
    putStrLn $ show . solve ( 1:numbers )

solve :: [Int] -> Int
solve (x:xs)
    | length (filter even xs) == length xs = return (x:xs)--(solve (map (/2) xs))
    | otherwise = return x
