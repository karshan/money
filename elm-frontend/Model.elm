module Model where

type alias Model =
    { transactions : List Transaction
    , currentFilter : String
    }
type alias Transaction =
    { description : String
    , date : String
    , amount : Int
    , tags : List String
    }
