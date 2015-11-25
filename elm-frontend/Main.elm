import Html exposing (Html)
import Http
import Effects exposing (Effects, Never)
import Json.Decode exposing (Decoder, object1, object4, list, string, int, (:=))
import Maybe exposing (withDefault)
import Model exposing (Model, Transaction)
import Signal exposing (Address)
import StartApp exposing (start)
import Task exposing (Task)
import View exposing (renderTransactions)

app =
    start { init = init, view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

type Action = LoadTransactions (List Transaction)

init : (Model, Effects Action)
init = (Model [], getTransactions)

view : Address Action -> Model -> Html
view _ {transactions} = renderTransactions transactions

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
