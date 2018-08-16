isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared size to 9")

applyLog :: (s, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

newtype Writer w a = Writer { runWriter :: (a, w)}
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x
                              in Writer (y, v `mappend` v')

import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

モナドとしての関数

instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w

import Control.Monad.Instances

addStuff :: Int -> Int
addStuff x = let
    a = (*2) x
    b = (+10) x
    in a+b

stack

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newSrtack1) = push 3 stack
    (a, newSrtack2) = pop newSrtack1
    in pop newSrtack2

newtyoe State s a = State { runState :: s -> (a,s)}

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State, g) = f a
                                    in g newState

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

乱数とstateモナド

import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)

エラーとMonad

instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)

applicativeとmonad
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)

入れ子になったmonadを平らにする。

join :: (Monad m) => m (m a) -> m a

冪集合
powerset :: [a] -> [[a]]
powerset xs  = filterM (\x -> [Truem false]) xs

foldM

foldl :: (a -> b -> c) -> a -> [b] -> a

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)
    