module View
       ( renderTransaction
       , renderTransactions
       ) where

import Model (Transaction)
import Inputs (transactionClicks)
import Graphics.Input (clickable)

text : String -> Element
text = plainText

showAmount : Int -> String
showAmount a = show (a `div` 100) ++ "." ++ show (a `rem` 100)

renderTransactions : [Transaction] -> Element
renderTransactions ts = let f x = clickable transactionClicks.handle (Just x) (renderTransaction x) in
                            flow down <| (text "HEADER")::map f ts

renderTransaction : Transaction -> Element
renderTransaction t = flow right [ width 100 <| text t.date
                                 , width 1286 <| text t.description
                                 , width 80 <| text <| showAmount t.amount
                                 , width 200 <| text <| "[" ++ (concat <| intersperse ", " t.tags) ++ "]"
                                 ]
