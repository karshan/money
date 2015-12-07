import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, style, placeholder)
import Html.Events exposing (on, targetValue)
import Http
import Effects exposing (Effects, Never)
import Json.Decode exposing (Decoder, object1, object4, list, string, int, (:=))
import Maybe exposing (withDefault)
import Model exposing (Model, Transaction, initModel)
import Signal exposing (Address, message)
import String exposing (contains, toLower, append)
import StartApp exposing (start)
import Task exposing (Task)
import View exposing (renderTransactions)
import List exposing (filter, sortBy, reverse, length)

app =
    start { init = init, view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

type Action = LoadTransactions (List Transaction) | Filter String | AddTag String

init : (Model, Effects Action)
init = (initModel, getTransactions)

inputStyle = [("width", "100%"), ("height", "2em"), ("border", "solid 1px gray")]
textStyle = [("text-align", "center"), ("padding", ".5em"), ("border", "solid 1px gray")]

view : Address Action -> Model -> Html
view address {transactions, currentFilter} =
    let filteredTransactions = filter (doFilter currentFilter) <| reverse <| sortBy .date transactions
    in div
        [
        ]
        [ input
            [ autofocus True
            , placeholder "filter"
            , style inputStyle
            , on "input" targetValue (message address << Filter)
            ]
            []
        , input
            [ placeholder "tag"
            , style inputStyle
            ,  on "input" targetValue (message address << AddTag)
            ]
            []
        , div [ style textStyle ]
            [ text (toString (length filteredTransactions) `append` " transactions") ]
        , renderTransactions filteredTransactions
        ]

doFilter : String -> Transaction -> Bool
doFilter s {description} = toLower s `contains` toLower description

update : Action -> Model -> (Model, Effects Action)
update action m = case action of
    (LoadTransactions ts) -> ({ m | transactions = ts }, Effects.none)
    (Filter s) -> ({ m | currentFilter = s }, Effects.none)
    (AddTag s) -> ({ m | addTag = s }, Effects.none)

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
