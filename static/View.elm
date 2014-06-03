module View
       ( renderTransaction
       , renderTransactions
       ) where

import Model (Transaction)

renderTransactions : [Transaction] -> Element
renderTransactions ts = flow down <| map renderTransaction ts

renderTransaction : Transaction -> Element
renderTransaction t = asText (t.description, t.amount, t.date, t.tags)
