module Model
       ( Transaction
       , fromJsonTransaction
       , transactions
       ) where

import Dict (get)
import Http (sendGet)
import Json
import JsonUtils (listFromJson, stringFromJson, intFromJson)
import Utils (responseToMaybe, maybeBind, maybeFmap)

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
transactions = let (>>=) = flip maybeBind in
      (\a -> responseToMaybe a >>= Json.fromString >>= listFromJson fromJsonTransaction) <~ (sendGet (constant "/transactions"))
