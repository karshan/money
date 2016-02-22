module Model where

import List   exposing (foldr, map)
import Util   exposing (Either (..), ciContains)

type NumOp = Lt | Gt | NumEq
type StringOp = CiContains
type BinaryOp = And | Or
type Expr =
    Not Expr
  | BinaryOp BinaryOp Expr Expr
  | StringOp StringOp String
  | NumOp NumOp Int

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
    , categorizers : List (Either String Expr, List String)
    , filter' : String
    , filterExpr : Either String Expr
    , addTag : String
    , error : Bool
    }
type alias Transaction =
    { description : String
    , date : String
    , amount : Int
    }

initModel : Model
initModel =
    { transactions = []
    , transactionsRev = ""
    , categorizers = []
    , filter' = ""
    , filterExpr = Left "initial filter"
    , addTag = ""
    , error = False
    }
