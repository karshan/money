module View where

import Html exposing (Html, div, table, tr, td, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Transaction, Action (..))
import String exposing (isEmpty)
import Signal exposing (Address, message)
import List exposing (map, map2, length)

trStyle = []
tdStyle = [("border", "solid 1px gray"), ("padding", ".5em")]
tableStyle = [("width", "100%"), ("border", "solid 1px gray"), ("border-collapse", "collapse")]

zip = map2 (,)

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

tagStyle = [ ("padding", ".3em")
           , ("background-color", "#1abc9c")
           , ("color", "#fff")
           , ("border-radius", ".3em")
           , ("margin", "0 .3em .3em 0")
           , ("cursor", "pointer")
           , ("transition", ".25s linear")
           ]

renderTag : Address Action -> String -> Html
renderTag address t =
    span
      [ style tagStyle
      , onClick address <| RemoveTag t
      ]
      [text t]
