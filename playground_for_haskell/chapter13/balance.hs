type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 first
    Nothing
    second <- landRight 2 first
    landLeft 1 second

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

リストモナド
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []

リストモナドの非決定性計算と、リスト内包表記は実は同じ。

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)
これは
[ (n, ch) | n <- [1,2], ch <- ['a', 'b']]
と同じ、リスト内包表記は非決定性計算の構文糖衣

リスト内包表記のように、filterをかけるにはMonadPlusとguardを知っておく必要がある。
[1..50] >>= (\x -> guard('7' `elem` show x) >> return x)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero