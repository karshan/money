module List.Util
  ( index
  , modifyAt
  )
  where

import List exposing (head, drop, indexedMap)

index : Int -> List a -> Maybe a
index n xs = head (drop n xs)

modifyAt : Int -> (a -> a) -> List a -> List a
modifyAt n f xs =
  indexedMap (\i a ->
    if i == n then
      f a
    else
      a) xs
