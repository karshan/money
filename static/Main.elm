import Graphics.Input (Input, input)
import Json
import Http (sendGet, Response, Success)
import Graphics.Input (clickable)
import Utils (fromMaybe, listFromJson, responseToMaybe, maybeBind, maybeFmap)
import Transaction (Transaction, transactionFromJson)

main : Signal Element
main = application <~ state

data State = Start | TList [Transaction] | Edit Transaction

state : Signal State
state = lift (fromMaybe Start) <| merges stateSignals

stateSignals : [Signal (Maybe State)]
stateSignals = [ maybeFmap TList <~ transactions
               , maybeFmap Edit <~ transactionClicks.signal
               ]

transactions : Signal (Maybe [Transaction])
transactions = let (>>=) = flip maybeBind in
      (\a -> responseToMaybe a >>= Json.fromString >>= listFromJson transactionFromJson) <~ (sendGet (constant "/transactions"))

transactionClicks : Input (Maybe Transaction)
transactionClicks = input Nothing

application : State -> Element
application st = flow down <| [ nav, container st ]

nav : Element
nav = text "NAVIGATION BAR"

container : State -> Element
container st = case st of
    TList ts -> renderTransactions ts
    Edit t -> renderTransaction t
    Start -> asText "START STATE"

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
