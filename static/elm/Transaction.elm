module Transaction
    ( Transaction
    , transactionFromJson
    ) where

import Utils (intFromJson, stringFromJson, listFromJson, maybeBind)
import Dict (get)
import Json

type Transaction = { description : String
                   , date : String -- TODO actual date type
                   , amount : Int
                   , tags : [String]
                   }

transactionFromJson : Json.Value -> Maybe Transaction
transactionFromJson a = case a of
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

