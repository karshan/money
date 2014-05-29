module Utils 
    ( responseToMaybe
    , maybeBind
    , maybeFmap)
    where

import Http (Response, Success)

responseToMaybe : Response a -> Maybe a
responseToMaybe a = case a of
    Success b -> Just b
    _ -> Nothing

maybeBind : (a -> Maybe b) -> Maybe a -> Maybe b
maybeBind f val = case val of
    Just a -> f a
    Nothing -> Nothing

maybeFmap : (a -> b) -> Maybe a -> Maybe b
maybeFmap f val = case val of
    Just a -> Just <| f a
    Nothing -> Nothing
