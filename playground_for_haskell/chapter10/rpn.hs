solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (y * x):ys
    where foldingFunction (x:y:ys) "+" = (y + x):ys
    where foldingFunction (x:y:ys) "-" = (y - x):ys
    where foldingFunction (x:y:ys) "/" = (y / x):ys
    where foldingFunction (x:y:ys) "^" = (y ** x):ys
    where foldingFunction (x:xs) "ln" = log x:xs
    where foldingFunction xs "sum" = [sum xs]
    where foldingFunction xs numberString = read nunberString:xs

