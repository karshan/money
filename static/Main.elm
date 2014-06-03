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

renderTransactions : [Transaction] -> Element
renderTransactions ts = flow down <| map renderTransaction ts

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
            (\ttdescription ->
                (\ttdate ->
                    (\ttamount ->
                        (\tttags ->
                            Just { description = ttdescription
                                 , date = ttdate
                                 , amount = ttamount
                                 , tags = tttags
                                 }
                        ) `maybeBind` ttags
                    ) `maybeBind` tamount
                ) `maybeBind` tdate
            ) `maybeBind` tdescription
    _ -> Nothing

transactions : Signal (Maybe [Transaction])
transactions = let (=<<) = maybeBind in
      (\a -> (listFromJson fromJsonTransaction) =<< (Json.fromString =<< responseToMaybe a)) <~ (sendGet (constant "/transactions"))

main : Signal Element
main = maybe (asText "ERROR") renderTransactions <~ transactions
