data Bool = False | True
dataキーワードは False Trueは値コンストラクタ　この型が取り得る値の種類を指定している。
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
このように型を定義して、
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
このような関数を定義した時、値コンストラクタは型ではないので、Circle -> Floatみたいなことは無理　True -> Floatが無理なのと同じ
値コンストラクタでパターンマッチをすることができる。Trueとか5とかに対してパターンマッチをしている場合もフィールド有りの場合も実はあんまり変わらない
型シグネチャ　→ :tで見れるやつ

CircleやRectangleのとる引数を型引数と呼ぶ。
Maybeだと
data Maybe a = Nothing | Just a
aが型引数になる。これに置いてMaybeは型コンストラクタと呼ばれる。
型と型コンストラクタは別物。単なるMaybeという型は存在しない。型コンストラクタは全ての引数を入れて初めて何かしらの値の型になれる。
型コンストラクタは引数をとって、その型を生成する関数のイメージ
CharをMaybeの引数に渡すと、Maybe Charという型が得られる。
例えば、Just "a"という値はMaybe Charという型をもつ。
データ型を宣言する時に=の前にあるのが型コンストラクタ、後ろにあるのが値コンストラクタ
型シノニム　type String = [Char]
type phoneBool = [(String, String)]
既存の型に対して、自分の認識しやすいように名前をつけること。

型クラスは特定の振る舞いを定義するもの。定義された通りに振舞うことができる型はその型クラスのインスタンスとなる。
例えばEq型クラス
clss Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)

ファンクターは全体を写せるものの型クラス　mapが使えるもの。
ファンクターの型クラスで定義されているfmapは
class Functor f where
  fmap :: (a -> b) -> f a -> f b
このようにfは具体型ではなく、型コンストラクタ
fmapは ある型aから別の型bへの関数　と　ある型aに適用されたファンクター値　をとり
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