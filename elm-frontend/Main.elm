import Html exposing (Html, table, tr, td, text)
import Http
import Effects exposing (Effects, Never)
import Json.Decode exposing (Decoder, object1, object4, list, string, int, (:=))
import Maybe exposing (withDefault)
import Signal exposing (Address)
import StartApp exposing (start)
import Task exposing (Task)

app =
    start { init = init, view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

type Action = LoadTransactions (List Transaction)
type alias Model =
    { transactions : List Transaction }
type alias Transaction =
    { description : String
    , date : String
    , amount : Int
    , tags : List String
    }

init : (Model, Effects Action)
init = (Model [], getTransactions)

view : Address Action -> Model -> Html
view _ {transactions} = renderTransactions transactions

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

update : Action -> Model -> (Model, Effects Action)
update (LoadTransactions ts) m = ({ m | transactions = ts }, Effects.none)

getTransactions : Effects Action
getTransactions =
    Http.get (list transaction) "https://karshan.me:8443/transactions"
      |> Task.toMaybe
      |> Task.map (LoadTransactions << withDefault [])
      |> Effects.task

transaction : Decoder Transaction
transaction =
    object4 Transaction
      ("description" := string)
      ("date" := string)
      ("amount" := int)
      ("tags" := list string)
