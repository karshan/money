import Http (sendGet, Success, Response)
import Json
import Dict (Dict, get)
import Utils (responseToMaybe, maybeBind, maybeFmap)
import JsonUtils (intFromJson, stringFromJson, listFromJson)

type Transaction = { description : String
                   , date : String -- TODO actual date type
                   , amount : Int
                   , tags : [String]
                   }

renderTransactions : Json.Value -> Maybe Element
renderTransactions a = maybeFmap (\ts -> (flow down) <| map renderTransaction ts) <| listFromJson fromJsonTransaction a

renderTransaction : Transaction -> Element
renderTransaction t = asText (t.description, t.amount, t.date, t.tags)

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

transactions : Signal (Response String)
transactions = sendGet (constant "/transactions")

main : Signal Element
main = maybe (asText "ERROR") id <~ (maybeBind renderTransactions
                                 <~ (maybeBind Json.fromString
                                 <~ (responseToMaybe <~ transactions)))
