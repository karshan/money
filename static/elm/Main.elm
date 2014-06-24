import Graphics.Input (Input, input)
import Json
import Http (sendGet, Response, Success)
import Graphics.Input (clickable)
import Utils (fromMaybe, listFromJson, responseToMaybe, maybeBind, maybeFmap)
import Transaction (Transaction, transactionFromJson)

data State = Start | TList [Transaction] | Edit Transaction

data Nav = Home

navI : Input Nav
navI = input Home

editTransaction : Input (Maybe Transaction)
editTransaction = input Nothing

main : Signal Element
main = application <~ appState

appState : Signal State
appState = lift (fromMaybe Start) <| merges [ maybeFmap TList <~ transactions
                                            , maybeFmap Edit <~ editTransaction.signal
                                            ]

transactions : Signal (Maybe [Transaction])
transactions = let (>>=) = flip maybeBind
                   refreshSignal = keepIf (\a -> a == Home) Home navI.signal in
      (\a -> responseToMaybe a >>= Json.fromString >>= listFromJson transactionFromJson) <~ sendGet ((\_ -> "/transactions") <~ refreshSignal)


application : State -> Element
application st = flow down <| [ nav st, container st ]

nav : State -> Element
nav _ = flow right <| [ clickable navI.handle Home <| text "Home" ]

container : State -> Element
container st = case st of
    TList ts -> renderTList ts
    Edit t -> renderEdit t
    _ -> text "Gimme a STATE yo!"

text : String -> Element
text = plainText

showAmount : Int -> String
showAmount a = show (a `div` 100) ++ "." ++ show (a `rem` 100)

renderTList : [Transaction] -> Element
renderTList ts = let renderT t = flow right [ width 100 <| text t.date
                                            , width 1286 <| text t.description
                                            , width 80 <| text <| showAmount t.amount
                                            , width 200 <| text <| "[" ++ (concat <| intersperse ", " t.tags) ++ "]"
                                            ]
                     f x = clickable editTransaction.handle (Just x) (renderT x) in
                            flow down <| {-(text "HEADER")::-}map f ts

renderEdit : Transaction -> Element
renderEdit t = asText t
