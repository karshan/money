module Model where

type alias Model =
    { transactions : List Transaction
    , transactionsRev : String
    , currentFilter : String
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
initModel = Model [] "" "" "" False
