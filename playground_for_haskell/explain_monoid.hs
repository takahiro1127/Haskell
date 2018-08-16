type IntList  = [Int]
typeは型シノニムを作るためのもの、既存の型に別名をつけて呼びやすくするためのもの。

newtype CharList = CharList {getCharList :: [Char]}
newtypeは既存の型を包んで新しい型を作るためのもの。型クラスのインスタンスを作りやすくするための物。
newtypeを使って既存の型を包むことより出来上がる型は物と型とは別物になる。

newtypeは値コンストラクタが１つだけ、フィールドも一つだけという制限のついたdata宣言とみなしてもOK

dataキーワードは自作の新しいデータ型を作るための物。

monoid 結合的な二項演算子を、その演算に関する単位元からなる。

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m 
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

monoidのインスタンスを作るときの決まり
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
最後の決まりは結合的であることを示している。

リストはmonoid　++と[]がモノイドをなす。
instance Monoid [a] where
    mempty = []
    mappend = (++)
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

ある型をfoldableにしたい場合はfoldmapさえ定義すれば良い。
instance F.Foldable Tree where
    foldmap f EmptyTree = menpty
    foldmap f (Node x l r) = F.foldmap f l `mappend`
                              f x `mappend`
                              F.foldmap f r

                              