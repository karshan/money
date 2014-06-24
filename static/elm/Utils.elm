module Utils
    ( responseToMaybe
    , maybeBind
    , maybeFmap
    , intFromJson
    , stringFromJson
    , listFromJson
    , fromMaybe
    ) where

import Http (Response, Success)
import Json

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

intFromJson : Json.Value -> Maybe Int
intFromJson a = case a of
    (Json.Number b) -> Just <| round b
    _ -> Nothing

stringFromJson : Json.Value -> Maybe String
stringFromJson a = case a of
    (Json.String a) -> Just a
    _ -> Nothing

listFromJson : (Json.Value -> Maybe a) -> Json.Value -> Maybe [a]
listFromJson fromJson a = case a of
    (Json.Array xs) -> Just <| justs <| map fromJson xs
    _ -> Nothing

fromMaybe : a -> Maybe a -> a
fromMaybe d m = case (d, m) of
    (_, Just a) -> a
    (a, Nothing) -> a
