{-# LANGUAGE OverloadedStrings #-}
module Util
    ( (?)
    , index
    , maybeRead
    , listApp
    , window
    , substr
    , count
    , mean
    , median
    , splitOnIndices
    ) where

import Data.List (foldl', sort)
import Data.Maybe (listToMaybe)

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:_) 0 = Just x
index (_:xs) n = index xs (n - 1)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

listApp :: (a -> a -> b) -> [a] -> b
listApp f ls = f (head ls) (last ls)

mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = head xs:map f (tail xs)

-- this eats elements to make sure the output has elements all of length n
window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs = if length xs < n then [] else take n xs:window n (tail xs)

substr :: Int -> Int -> [a] -> [a]
substr l u = take (u - l) . drop l

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

mean :: Floating a => [a] -> a
mean = fst . foldl' (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0)

median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
         | otherwise = undefined -- This never happens but not including this case causes non-exhaustive warning. I wonder if this can be solved in dependently typed languages.
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

--TODO is should be a Set Int not a [Int]
--TODO flip some burgers and get rid of the lambda
-- works like Data.List.Split.splitOn, eats the elements at the indices to split on
splitOnIndices :: [Int] -> [a] -> [[a]]
splitOnIndices is xs = filter (not . null) $ mapTail (drop (1 :: Int)) $ map (\a -> listApp substr a xs) $ window 2 is'
    where
        is' = 0:is ++ [length xs]