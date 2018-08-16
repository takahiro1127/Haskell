
main :: IO()
main = do
    [b, c] <- map read . words <$> getLine
    if (b * c) `mod` 2 == 0
        then putStrLn "Even"
        else putStrLn "Odd"

