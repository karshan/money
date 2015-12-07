module View where

import Html exposing (Html, div, table, tr, td, text)
import Html.Attributes exposing (style)
import Model exposing (Transaction)
import String exposing (isEmpty)

trStyle = []
tdStyle = [("border", "solid 1px gray")]
tableStyle = [("width", "100%"), ("border", "solid 1px gray"), ("border-collapse", "collapse")]

renderTransactions : List Transaction -> Html
renderTransactions transactions =
    let header = tr [style trStyle]
                       [ td [style tdStyle] [text "date"]
                       , td [style tdStyle] [text "description"]
                       , td [style tdStyle] [text "amount"]
                       , td [style tdStyle] [text "tags"]
                       ]
    in table [style tableStyle] <| header::List.map renderTransaction transactions

renderTransaction : Transaction -> Html
renderTransaction {description, date, amount, tags} =
    tr [style trStyle]
          [ td [style tdStyle] [text date]
          , td [style tdStyle] [text description]
          , td [style tdStyle] [text (toString <| (-1 * (toFloat amount))/100)]
          , td [style tdStyle] [text (toString tags)]
          ]
