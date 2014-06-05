module View
       ( renderTransaction
       , renderTransactions
       , transactionClicks
       ) where

import Model (Transaction)
import Graphics.Input (Input, input, clickable)

transactionClicks : Input Transaction
transactionClicks = input { description = "None", date = "1/1/1970", amount = 0, tags = [] }

text : String -> Element
text = plainText

showAmount : Int -> String
showAmount a = show (a `div` 100) ++ "." ++ show (a `rem` 100)

renderTransactions : [Transaction] -> Element
renderTransactions ts = let f x = clickable transactionClicks.handle
                                  x (renderTransaction x) in
                            flow down <| (text "HEADER")::map f ts

renderTransaction : Transaction -> Element
renderTransaction t = flow right [ width 100 <| text t.date
                                 , width 1286 <| text t.description
                                 , width 80 <| text <| showAmount t.amount
                                 , width 200 <| text <| "[" ++ (concat <| intersperse ", " t.tags) ++ "]"
                                 ]
