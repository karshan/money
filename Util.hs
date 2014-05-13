module Util
    ( (<&>)
    , (>>>)
    , (&)
    , (?)
    , maybeRead
    , listApp
    , window
    , substr
    , count
    , splitOnIndices
    ) where

import Data.Maybe

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

--TODO rename to |>
--TODO pick one $ or & don't use both....
(&) :: a -> (a -> b) -> b
(&) = flip ($)

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

listApp :: (a -> a -> b) -> [a] -> b
listApp f ls = f (head ls) (last ls)

mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = head xs:map f (tail xs)

window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs = if length xs < n then [] else take n xs:window n (tail xs)

substr :: Int -> Int -> [a] -> [a]
substr l u = take (u - l) . drop l

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

--TODO flip some burgers and get rid of the lambda
splitOnIndices :: [Int] -> [a] -> [[a]]
splitOnIndices is xs = window 2 is' & map (\a -> listApp substr a xs) & mapTail (drop (1 :: Int))
    where
        is' = 0:is ++ [length xs]
