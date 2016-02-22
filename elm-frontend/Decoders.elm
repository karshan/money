module Decoders where

import Json.Decode exposing (Decoder, object3, string, int, list, (:=))
import Model       exposing (Transaction)

transactions : Decoder (List Transaction)
transactions = list transaction

transaction : Decoder Transaction
transaction =
    object3 Transaction
      ("description" := string)
      ("date" := string)
      ("amount" := int)
