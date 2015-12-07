import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, style, placeholder)
import Html.Events exposing (onKeyPress, on, targetValue)
import Http
import Effects exposing (Effects, Never)
import Json.Decode exposing (Decoder, tuple2, object1, object4, list, string, int, (:=))
import Maybe exposing (withDefault)
import Model exposing (Model, Transaction, initModel)
import Signal exposing (Address, message)
import String exposing (contains, left, toLower, append)
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

type Action = LoadTransactions (String, List Transaction)
            | Filter String
            | AddTag String
            | PerformAddTag
            | NoOp

init : (Model, Effects Action)
init = (initModel, getTransactions)

inputStyle = [("width", "100%"), ("height", "2em"), ("border", "solid 1px gray")]
textStyle = [("text-align", "center"), ("padding", ".5em"), ("border", "solid 1px gray")]

view : Address Action -> Model -> Html
view address {transactions, transactionsRev, currentFilter, addTag} =
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
            [ placeholder "add tag"
            , style inputStyle
            , on "input" targetValue (message address << AddTag)
            , onKeyPress address (\k -> if k == 13 {- enter -} then PerformAddTag else NoOp)
            ]
            []
        , div [ style textStyle ]
            [ text ((toString (length filteredTransactions) `append` " transactions")
                    `append` " (rev " `append` (left 10 transactionsRev) `append` ")")]
        , renderTransactions filteredTransactions
        ]

doFilter : String -> Transaction -> Bool
doFilter s {description} = toLower s `contains` toLower description

update : Action -> Model -> (Model, Effects Action)
update action m = case action of
    (LoadTransactions (rev, ts)) -> ({ m | transactions = ts, transactionsRev = rev }, Effects.none)
    (Filter s) -> ({ m | currentFilter = s }, Effects.none)
    (AddTag s) -> ({ m | addTag = s }, Effects.none)
    PerformAddTag -> (performAddTag m, Effects.none)
    NoOp -> (m, Effects.none)

performAddTag : Model -> Model
performAddTag m =
    { m | transactions =
            List.map (\t -> if doFilter m.currentFilter t
                            then { t | tags = t.tags `List.append` [m.addTag] }
                            else t) m.transactions}

getTransactions : Effects Action
getTransactions =
    Http.get (tuple2 (,) string (list transaction)) "https://karshan.me:8443/transactions"
      |> Task.toMaybe
      |> Task.map (LoadTransactions << withDefault ("", []))
      |> Effects.task

transaction : Decoder Transaction
transaction =
    object4 Transaction
      ("description" := string)
      ("date" := string)
      ("amount" := int)
      ("tags" := list string)
