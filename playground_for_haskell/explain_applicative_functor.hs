applicativeの前に普通のfanctor

ファンクターは全体を写せるものの型クラス　mapが使えるもの。
ファンクターの型クラスで定義されているfmapは
class Functor f where
  fmap :: (a -> b) -> f a -> f b
このようにfは具体型ではなく、型コンストラクタ
fmapは ある型aから別の型bへの関数　と　ファンクター型の値a　をとり
別の型bの方に適用されたファンクター値を返す関数。
ここで普通のmap関数の型シグネチャを見ると 
map :: (a -> b) -> [a] -> [b]
となっている。
mapは ある型から別の型への関数　と ある型のリストをとり、　別の型のリストを返す関数

リストに対するファンクターのインスタンス宣言は
instance Functor [] where
  fmap = map
Maybeに対するファンクターのインスタンス宣言は
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
ファンクターのインスタンスになれるのは箱のような働きをする型、リストの場合はなかがからだったり、箱だったり、値だったりする。MaybeもNothingだったり、値だったり、またさらに箱が入っていたりする。Treeもそう

applicative fanctor

-- 2引数関数でファンクターを使うと、fmapの引数の関数が中に入る　ちょっと雑すぎる文章
:t fmap (++) (Just "hey")
fmap (++) (Just "hey") :: Maybe([Char -> Char])  [Char　-> Char]の部分が関数

このようにすると、
let a = fmap (*) [1,2,3,4]
fmap (\f -> f 9) a
[9,18,27,36]
みたいな感じで、mapしたい関数の中身ごと取れる。

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
    pure = Just
    Nothing <*> _  = Nothing
    (Just f) <*> something = fmap f something
  
fmap f x <*> y <*> ... という書き方はよくするので、これように<$>というfmapと等価な中置演算子が用意されている。
f <$> x = fmap f x
というように使う。

リストのapplicative
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs ]

fsから各関数を取り出して、xのそれぞれに対して適用させて行く。

IOのapplicative
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return fx

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

また関数もapplicativeである。

instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

[(+2), (*3)] <*> [1,2] で [3,6]を返すようなインスタンスが欲しい

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (ZipWIth (\f x -> f x) fs xs)

連携しているapplicativeの便利な関数

listA2 :: (Applicative f) => ( a -> b -> c ) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

これによって、
liftA2 (:) (Just 3) <*> (Just [4])
はJust[3,4]になる。
liftA2 (+) [3,4] [2,7]
[5,10,6,11]
( a -> b -> c ) -> (f a -> f b -> f c)
として見れば、通常の2引数関数を、2つのアプリカティブ値を引数にとる関数に昇格させてるイメージ。

好きなアプリカティブ値から、それらの返り値をリストにしたものをもる１つのアプリカティブ値を返す関数を作る。
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
もしくは
sequenceA = foldr (listA2 (:)) (pure [])

このようになる。
sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]

sequenceA [(+3), (+2), (+1)] 3
[6,5,4]

sequenceA [(+3), (*2)] 2
[5,4]

sequenceA [(<3), (>=2), even] 2
[True,True,True]

and $ sequenceA [(<3), (>=2), even] 2
True