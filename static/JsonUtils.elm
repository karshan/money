module JsonUtils
    ( intFromJson
    , stringFromJson
    , listFromJson
    )
    where

import Json

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
