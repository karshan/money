module View where

import Html exposing (Html, node, input, div, table, tr, td, text, span)
import Html.Attributes exposing (autofocus, style, placeholder, name, content)
import Html.Events exposing (onKeyPress, onClick, on, targetValue)
import Model exposing (Expr, Model, Transaction, Action (..))
import Filter exposing (eval)
import Set exposing (Set)
import String exposing (isEmpty, left)
import Signal exposing (Address, message)
import List exposing (concatMap, map, length, filter, reverse, sortBy)
import Util exposing (Either (..), either, zip)

inputStyle = [("width", "100%"), ("height", "4em"), ("border", "solid 1px gray")]
errorInputStyle = inputStyle ++ [("color", "red")]

textStyle = [("text-align", "center"), ("padding", ".5em"), ("border", "solid 1px gray")]
errorTextStyle = textStyle ++ [("color", "red")]

trStyle = []
tdStyle = [("border", "solid 1px gray"), ("padding", ".5em")]
tableStyle = [("width", "100%"), ("border", "solid 1px gray"), ("border-collapse", "collapse")]

tagStyle = [ ("padding", ".3em")
           , ("background-color", "#1abc9c")
           , ("color", "#fff")
           , ("border-radius", ".3em")
           , ("margin", "0 .3em .3em 0")
           , ("cursor", "pointer")
           ]

inputBox : String -> (String -> Signal.Message) -> List Html.Attribute -> List (String, String) -> Html
inputBox s f attrs st =
    input ([ autofocus True
          , placeholder s
          , style st
          , on "input" targetValue f
          ] ++ attrs)
          []

mkTable : List (List Html) -> Html
mkTable xss  =
    table [style tableStyle] <| map (\(i, xs) -> tr [style trStyle] <| map (\(j, x) ->
        td [style tdStyle] [x])
            <| zip [1..length xs] xs)
            <| zip [1..length xss] xss

renderTransactions : Address Action -> List (Transaction, Set String) -> Html
renderTransactions address transactions =
    mkTable <| [map text ["date", "description", "amount", "tags"]]
             ++ map (renderTransaction address) transactions

renderTransaction : Address Action -> (Transaction, Set String) -> List Html
renderTransaction address ({description, date, amount}, tags) =
    let stringAmount = toString (-amount // 100) ++ "." ++ pad (toString ((abs amount) `rem` 100))
        pad n = if String.length n == 1 then n ++ "0" else n
        amountDiv = div [style [("text-align", "right")]] [text stringAmount]
    in  [text date, text description, amountDiv] ++ [div [] <| map (renderTag address) []]

renderTag : Address Action -> String -> Html
renderTag address t =
    span
      [ style tagStyle
      , onClick address <| RemoveTag t
      ]
      [text t]

categorize : List (Either String Expr, List String) -> List Transaction -> List (Transaction, Set String)
categorize cats ts =
    let g : Transaction -> (Either String Expr, List String) -> Set String
        g tr (expr, tags) = if evalFilter expr tr then Set.fromList tags else Set.empty
        f : Transaction -> (Transaction, Set String)
        f t = (t, List.foldr Set.union Set.empty <| map (g t) cats)
    in  map f ts

evalFilter : Either String Model.Expr -> Transaction -> Bool
evalFilter me t =
    case me of
        Right e  -> eval e t
        Left _ -> True

view : Address Action -> Model -> Html
view address m =
    let filteredTransactions = filter (evalFilter m.filterExpr << fst) <| reverse <| sortBy (.date << fst) <| categorize m.categorizers m.transactions
        filterBox = inputBox "filter" (message address << Filter) [onKeyPress address (\k -> if k == 13 then FilterEnter else NoOp)] inputStyle
        addTagBox = inputBox "add tag" (message address << AddTag) [onKeyPress address (\k -> if k == 13 {- enter -} then PerformAddTag else NoOp)] inputStyle
    in div
        []
        [ node "meta" [name "viewport", content "width=device-width, initial-scale=1.0, maximum-scale=1.0"] []
        , filterBox
        , addTagBox
        , div [ style textStyle ]
              [ text ((toString (length filteredTransactions) ++ " transactions")
                    ++ " (rev " ++ (left 6 m.transactionsRev) ++ ")")]
        , div [ style errorTextStyle ] [ text (if m.error then "error: " ++ (toString m.error) else "") ]
        , div [ style textStyle ] [ text <| either identity toString <| m.filterExpr ]
        , renderTransactions address filteredTransactions
        ]
