data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeTop :: Directions -> Tree Char -> Tree Char
changeTop (L:ds) (Node x l r) = Node x (changeTop ds l) r
changeTop (R:ds) (Node x l r) = Node x l (changeTop ds r)
changeTop [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

type Breadcrumbs = [Crumb a]

-- goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
-- goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
goLeft :: Zipper  -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper  -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

-- goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
-- goRifht (Node x l r, bs) = (r, RightCrumb x l:bs)

x -: f = f x

data Crumb a = LeftCrumb a (Tree a)
              | RightCrumb a (Tree a) deriving (Show)

-- goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
-- goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
-- goUp (t, RightCrumb x l:bs) = (Node x l t, bs)
goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Emptym bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost(goUp z)
