module Model where

import List exposing (foldr, map)
import Util exposing (ciContains)

type Action = LoadTransactions (String, List Transaction)
            | Filter String
            | AddTag String
            | PerformAddTag
            | AddTagResponse Bool
            | RemoveTag String
            | NoOp

type alias Model =
    { transactions : List Transaction
    , transactionsRev : String
    , filter' : String
    , addTag : String
    , error : Bool
    }
type alias Transaction =
    { description : String
    , date : String
    , amount : Int
    , tags : List String
    }

initModel : Model
initModel =
    { transactions = []
    , transactionsRev = ""
    , filter' = ""
    , addTag = ""
    , error = False
    }
