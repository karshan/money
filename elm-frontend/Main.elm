import Html exposing (Html, div, input)
import Html.Attributes exposing (autofocus)
import Html.Events exposing (on, targetValue)
import Http
import Effects exposing (Effects, Never)
import Json.Decode exposing (Decoder, object1, object4, list, string, int, (:=))
import Maybe exposing (withDefault)
import Model exposing (Model, Transaction)
import Signal exposing (Address, message)
import String exposing (contains, toLower)
import StartApp exposing (start)
import Task exposing (Task)
import View exposing (renderTransactions)
import List exposing (filter, sortBy, reverse)

app =
    start { init = init, view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

type Action = LoadTransactions (List Transaction) | Filter String

init : (Model, Effects Action)
init = (Model [] "", getTransactions)

view : Address Action -> Model -> Html
view address {transactions, currentFilter} =
    div
      [
      ]
      [ input
          [ autofocus True
          , on "input" targetValue (message address << Filter)
          ]
          []
      , renderTransactions <| filter (doFilter currentFilter) <| reverse <| sortBy .date transactions
      ]

doFilter : String -> Transaction -> Bool
doFilter s {description} = toLower s `contains` toLower description

update : Action -> Model -> (Model, Effects Action)
update action m = case action of
    (LoadTransactions ts) -> ({ m | transactions = ts }, Effects.none)
    (Filter s) -> ({ m | currentFilter = s }, Effects.none)

getTransactions : Effects Action
getTransactions =
    Http.get (list transaction) "http://karshan.me:3000/transactions"
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
