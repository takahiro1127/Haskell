readMaybe :: (Read a ) => String -> Maybe
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing
                  
foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:xs:ys) "*" = return ((y * x): ys)
foldingFunction (x:xs:ys) "+" = return ((y + x): ys)
foldingFunction (x:xs:ys) "*" = return ((y - x): ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

import Data.List

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result
    
