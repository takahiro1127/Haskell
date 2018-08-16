doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2
boomBangs xs = [ if x < 10 then "BOOM!!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
factorical :: Integer -> Integer
factorical n = product [1..n]
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
replicate' :: Int -> a -> [a]
replicate' n x
    | n < 0 = []
    | otherwise = x : replicate' (n-1) x
take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
repeat' :: a -> [a]
repeat' x = x : repeat' x
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _  = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
-- elem' :: (Eq a) => a -> [a] -> Bool
-- elem' a [] = False
-- elem' a (x:xs)
--   | a == x = True
--   | otherwise = a `elem'` xs
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger
cylinder :: Double -> Double -> Double
cylinder r h = let sideArea = 2 * pi * r * h; topArea = pi * r ^ 2 in sideArea + 2 * topArea
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w/h ^ 2, bmi > 25.0]
compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100
multThree :: Int -> Int -> Int -> Int
multThree x y z  = x * y * z
dividedByTen :: (Floating a) => a -> a
dividedByTen = (/10)
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
applyTwice :: (a-> a) -> a -> a
applyTwice f x = f (f x)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _  [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x
-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' _ [] = []
-- filter' p (x:xs)
--     | p x  = x : filter p xs
--     | otherwise = filter p xs
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999 ..])
    where p x  = x `mod` 3829 == 0
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n * 3 + 1)
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
sum' :: (Num a) => [a] -> a
sum'  = foldl (+) 0
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then  True else acc) False ys 
maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max
reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []
reverse'r :: [a] -> [a]
reverse'r = foldl (flip (:)) []
filter' :: (a -> Bool) -> [a] -> [a] 
filter' p = foldr (\x acc -> if p x then x : acc else acc) []
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) +1

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

