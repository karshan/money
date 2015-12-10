module Model where

type Action = LoadTransactions (String, List Transaction)
            | Filter String
            | AmountFilter String
            | TagFilter String
            | AddTag String
            | PerformAddTag
            | AddTagResponse Bool
            | RemoveTag String
            | NoOp

type alias Model =
    { transactions : List Transaction
    , transactionsRev : String
    , filter' : String
    , amountFilter : String
    , tagFilter : String
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
    , amountFilter = ""
    , tagFilter = ""
    , addTag = ""
    , error = False
    }
