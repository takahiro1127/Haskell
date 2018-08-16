>>= はmonad値と普通の値を取る函数を引数にとり、その関数をモナド値に適用して、モナド値をとる。
Maybe a型の値とMaybe b型の値を返す関数を引数にとり、どうにかしてMaybe aに適用してくれる関数。
applyMaybe :: Maybe a -> Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = fx
Monad型クラス
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg = error msg

monad値は、文脈、失敗などを含んだ値。
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

