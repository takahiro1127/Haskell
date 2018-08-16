module Figure (
  Point(..)
, Shape(..)
, Person(..)
, area
, nudge
) where
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b  = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

data Person = Person { firstName :: String
                      , lastName :: String
                      , age ::  Int
                      , height :: Float
                      , phoneNumber :: String
                      , flavor :: String } deriving (Show, Eq, Read)
data Car = Car { company :: String
                , model :: String
                , year :: Int
                } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

phoneBook :: [(String, String)]
phoneBook = [("betty", "555-5959")
            , ("bonnie", "232-2020")
            , ("takhie", "282-3048")
            , ("greagrea", "383-3924")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- infixr 5 :-:
-- infixr 5 ^++
-- (^++) :: [a] -> [a] -> [a]
-- [] ^++ ys = ys
-- (x:-:xs) ^++ ys = x :-: (xs ^++ ys)
-- data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right
