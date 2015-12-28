module Model where

import List   exposing (foldr, map)
import Util   exposing (ciContains)

type NumOp = Lt | Gt | NumEq
type StringOp = CiContains
type BinaryOp = And | Or
type Expr =
    Not Expr
  | BinaryOp BinaryOp Expr Expr
  | StringOp StringOp String
  | NumOp NumOp Int
  | TagOp String

type Action = LoadTransactions (String, List Transaction)
            | LoadCategorizers (List (String, List String))
            | Filter String
            | FilterEnter
            | AddTag String
            | PerformAddTag
            | AddTagResponse Bool
            | RemoveTag String
            | NoOp

type alias Model =
    { transactions : List Transaction
    , transactionsRev : String
    , categorizers : List (Maybe Expr, List String)
    , filter' : String
    , filterExpr : Maybe Expr
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
    , categorizers = []
    , filter' = ""
    , filterExpr = Nothing
    , addTag = ""
    , error = False
    }
