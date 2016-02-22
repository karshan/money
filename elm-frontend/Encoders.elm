module Encoders where

import Json.Encode exposing (Value, string, object, int, list)
import List        exposing (map)
import Model       exposing (Transaction)

transaction : Transaction -> Value
transaction t = 
    object
        [ ("amount", int t.amount)
        , ("date", string t.date)
        , ("description", string t.description)
        ]

transactions : List Transaction -> Value
transactions = list << map transaction
