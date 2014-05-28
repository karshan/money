import Http (sendGet, Success, Response)
import Json (fromString, Value, Array, Object)
import Dict (Dict)

transactions : Signal (Response String)
transactions = sendGet (constant "/transactions")

main : Signal Element
main = maybe (asText "ERROR") id <~ (responseJson renderTransactions <~ transactions)

maybeBind : (a -> Maybe b) -> Maybe a -> Maybe b
maybeBind f val = case val of
    Just a -> f a
    Nothing -> Nothing

maybeFmap : (a -> b) -> Maybe a -> Maybe b
maybeFmap f val = case val of
    Just a -> Just <| f a
    Nothing -> Nothing

responseJson : (Value -> Maybe Element) -> (Response String) -> Maybe Element
responseJson f a = case a of
    (Success ts) -> maybeBind f <| fromString ts
    otherwise -> Nothing

mapArrayObject : (Dict String Value -> Maybe b) -> Value -> Maybe [b]
mapArrayObject f v = case v of
    (Array vs) -> Just <| justs <| map (\a -> case a of
                                        (Object o) -> f o
                                        otherwise -> Nothing) vs 
    otherwise -> Nothing

renderTransactions : Value -> Maybe Element
renderTransactions a = maybeFmap (flow down) <| mapArrayObject renderTransaction a 

renderTransaction : (Dict String Value -> Maybe Element)
renderTransaction t = Just <| asText t
