module Model where

type alias Model =
    { transactions : List Transaction }
type alias Transaction =
    { description : String
    , date : String
    , amount : Int
    , tags : List String
    }
