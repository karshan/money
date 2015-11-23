module Data.List.Util where

import           Data.Maybe (listToMaybe)
import           Prelude    hiding (head, (!!))

head :: [a] -> Maybe a
head = listToMaybe

last :: [a] -> Maybe a
last = head . reverse

(!!) :: [a] -> Int -> Maybe a
[] !! _ = Nothing
(x:_) !! 0 = Just x
(_:xs) !! n = xs !! (n - 1)
