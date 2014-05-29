import Http (sendGet, Success, Response)
import Json
import Dict (Dict, get)

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

responseJson : (Json.Value -> Maybe a) -> (Response String) -> Maybe a
responseJson f a = case a of
    (Success ts) -> maybeBind f <| Json.fromString ts
    _ -> Nothing

renderTransactions : Json.Value -> Maybe Element
renderTransactions a = maybeFmap (\ts -> (flow down) <| map renderTransaction ts) <| listFromJson fromJsonTransaction a

renderTransaction : Transaction -> Element
renderTransaction t = asText (t.description, t.amount, t.date, t.tags)

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

type Transaction = { description : String
                   , date : String -- TODO actual date type
                   , amount : Int
                   , tags : [String]
                   }
fromJsonTransaction : Json.Value -> Maybe Transaction
fromJsonTransaction a = case a of
    (Json.Object o) ->
        let
            tdescription = maybeBind stringFromJson <| get "description" o
            tdate = maybeBind stringFromJson <| get "date" o
            tamount = maybeBind intFromJson <| get "amount" o
            ttags = maybeBind (listFromJson stringFromJson) <| get "tags" o
        in
            maybeBind (\ttdescription ->
                maybeBind (\ttdate ->
                    maybeBind (\ttamount ->
                        maybeBind (\tttags ->
                            Just { description = ttdescription
                                 , date = ttdate
                                 , amount = ttamount
                                 , tags = tttags
                                 }) ttags
                              ) tamount
                          ) tdate
                      ) tdescription
    _ -> Nothing
