module View where

import Html exposing (Html, div, table, tr, td, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Transaction, Action (..))
import String exposing (isEmpty)
import Signal exposing (Address, message)

trStyle = []
tdStyle = [("border", "solid 1px gray"), ("padding", ".5em")]
tableStyle = [("width", "100%"), ("border", "solid 1px gray"), ("border-collapse", "collapse")]

renderTransactions : Address Action -> List Transaction -> Html
renderTransactions address transactions =
    let header = tr [style trStyle]
                       [ td [style tdStyle] [text "date"]
                       , td [style tdStyle] [text "description"]
                       , td [style tdStyle] [text "amount"]
                       , td [style tdStyle] [text "tags"]
                       ]
    in table [style tableStyle] <| header::List.map (renderTransaction address) transactions

renderTransaction : Address Action -> Transaction -> Html
renderTransaction address {description, date, amount, tags} =
    tr [style trStyle]
          [ td [style tdStyle] [text date]
          , td [style tdStyle] [text description]
          , td [style tdStyle] [text (toString <| (-1 * (toFloat amount))/100)]
          , td [style tdStyle] <| List.map (renderTag address) tags
          ]

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
