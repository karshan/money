module View where

import Html exposing (Html, table, tr, td, text)
import Model exposing (Transaction)

renderTransactions : List Transaction -> Html
renderTransactions transactions =
    let header = tr [] [ td [] [text "date"]
                       , td [] [text "description"]
                       , td [] [text "amount"]
                       , td [] [text "tags"]
                       ]
    in table [] <| header::List.map renderTransaction transactions

renderTransaction : Transaction -> Html
renderTransaction {description, date, amount, tags} =
    tr [] [ td [] [text date]
          , td [] [text description]
          , td [] [text (toString amount)]
          , td [] [text (toString tags)]
          ]
