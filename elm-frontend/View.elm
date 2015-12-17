module View where

import Html exposing (Html, node, input, div, table, tr, td, text, span)
import Html.Attributes exposing (autofocus, style, placeholder, name, content)
import Html.Events exposing (onKeyPress, onClick, on, targetValue)
import Model exposing (Model, Transaction, Action (..), doFilter)
import String exposing (isEmpty, left)
import Signal exposing (Address, message)
import List exposing (map, map2, length, filter, reverse, sortBy)
import AmountFilter exposing (parseSuccess)

zip : List a -> List b -> List (a, b)
zip = map2 (,)

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

renderTransactions : Address Action -> List Transaction -> Html
renderTransactions address transactions =
    mkTable <| [map text ["date", "description", "amount", "tags"]]
             ++ map (renderTransaction address) transactions

renderTransaction : Address Action -> Transaction -> List Html
renderTransaction address {description, date, amount, tags} =
    let stringAmount = toString (-amount // 100) ++ "." ++ pad (toString ((abs amount) `rem` 100))
        pad n = if String.length n == 1 then n ++ "0" else n
        amountDiv = div [style [("text-align", "right")]] [text stringAmount]
    in  [text date, text description, amountDiv] ++ [div [] <| map (renderTag address) tags]

renderTag : Address Action -> String -> Html
renderTag address t =
    span
      [ style tagStyle
      , onClick address <| RemoveTag t
      ]
      [text t]

view : Address Action -> Model -> Html
view address m =
    let filteredTransactions = filter (doFilter m) <| reverse <| sortBy .date m.transactions
        filterBox = inputBox "filter" (message address << Filter) [] inputStyle
        amountFilter = inputBox "amount filter" (message address << AmountFilter) [] <| if parseSuccess m.amountFilter then inputStyle else errorInputStyle
        tagFilter = inputBox "tag filter" (message address << TagFilter) [] inputStyle
        addTagBox = inputBox "add tag" (message address << AddTag) [onKeyPress address (\k -> if k == 13 {- enter -} then PerformAddTag else NoOp)] inputStyle
    in div
        []
        [ node "meta" [name "viewport", content "width=device-width, initial-scale=1.0, maximum-scale=1.0"] []
        , mkTable [[filterBox, amountFilter, tagFilter]]
        , addTagBox
        , div [ style textStyle ]
              [ text ((toString (length filteredTransactions) ++ " transactions")
                    ++ " (rev " ++ (left 6 m.transactionsRev) ++ ")")]
        , div [ style errorTextStyle ] [ text (if m.error then "error: " ++ (toString m.error) else "") ]
        , renderTransactions address filteredTransactions
        ]


