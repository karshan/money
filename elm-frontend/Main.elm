import Html exposing (Html, node, div, input, text)
import Html.Attributes exposing (autofocus, style, placeholder, name, content)
import Html.Events exposing (onKeyPress, on, targetValue)
import Http
import Effects exposing (Effects, Never)
import Json.Decode exposing (Decoder, tuple2, object1, object4, list, string, int, bool, (:=))
import Json.Encode exposing (encode)
import Maybe exposing (withDefault)
import Model exposing (Model, Transaction, Action (..), initModel)
import Signal exposing (Address, message)
import String exposing (contains, left, toLower)
import StartApp exposing (start)
import Task exposing (Task)
import View exposing (renderTransactions, mkTable)
import List exposing (filter, foldr, map, sortBy, reverse, member, length)
import AmountFilter exposing (doAmountFilter, parseSuccess)

baseUrl = "https://karshan.me:8443/"
-- baseUrl = "http://localhost:3000/"

app =
    start { init = init, view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

init : (Model, Effects Action)
init = (initModel, getTransactions)

inputStyle = [("width", "100%"), ("height", "4em"), ("border", "solid 1px gray")]
errorInputStyle = inputStyle ++ [("color", "red")]

textStyle = [("text-align", "center"), ("padding", ".5em"), ("border", "solid 1px gray")]
errorTextStyle = textStyle ++ [("color", "red")]

inputBox : String -> (String -> Signal.Message) -> List Html.Attribute -> List (String, String) -> Html
inputBox s f attrs st =
    input ([ autofocus True
          , placeholder s
          , style st
          , on "input" targetValue f
          ] ++ attrs)
          []

view : Address Action -> Model -> Html
view address m =
    let filteredTransactions = filter (doFilter m) <| reverse <| sortBy .date m.transactions
        filterBox = inputBox "filter" (message address << Filter) [] inputStyle
        amountFilter = inputBox "amount filter" (message address << AmountFilter) [] <| if parseSuccess m.amountFilter then inputStyle else errorInputStyle
        tagFilter = inputBox "tag filter" (message address << TagFilter) [] inputStyle
        addTagBox = inputBox "add tag" (message address << AddTag) [onKeyPress address (\k -> if k == 13 {- enter -} then PerformAddTag else NoOp)] inputStyle
    in div
        []
        [ node "meta" [name "viewport", content "width=device-width, initial-scale=1.0, maximum-scale=1.0"] []
        , mkTable [[filterBox, amountFilter, tagFilter]]
        , addTagBox
        , div [ style textStyle ]
              [ text ((toString (length filteredTransactions) ++ " transactions")
                    ++ " (rev " ++ (left 6 m.transactionsRev) ++ ")")]
        , div [ style errorTextStyle ] [ text (if m.error then "error: " ++ (toString m.error) else "") ]
        , renderTransactions address filteredTransactions
        ]

ciContains : String -> String -> Bool
ciContains a b = toLower a `contains` toLower b

doFilter : Model -> Transaction -> Bool
doFilter m {description, amount, tags} =
    m.filter' `ciContains` description &&
    doAmountFilter m.amountFilter (-1 * amount) &&
    if m.tagFilter == "" then True else (foldr (||) False (map (\x -> m.tagFilter `ciContains` x) tags))

update : Action -> Model -> (Model, Effects Action)
update action m = case action of
    (LoadTransactions (rev, ts)) -> ({ m | transactions = ts, transactionsRev = rev }, Effects.none)
    (Filter s) -> ({ m | filter' = s }, Effects.none)
    (AmountFilter s) -> ({ m | amountFilter = s }, Effects.none)
    (TagFilter s) -> ({ m | tagFilter = s }, Effects.none)
    (AddTag s) -> ({ m | addTag = s }, Effects.none)
    PerformAddTag -> (m, performAddTag m)
    AddTagResponse b -> if b then (m, getTransactions) else ({ m | error = True }, Effects.none)
    RemoveTag s -> (m, performRemoveTag m s)
    NoOp -> (m, Effects.none)

performAddTag : Model -> Effects Action
performAddTag m =
    Http.send Http.defaultSettings
        { verb = "PUT"
        , headers = [("Content-Type", "application/json")]
        , url = baseUrl ++ "addTags"
        , body = Http.string <| encode 0 <| Json.Encode.list <| List.map Json.Encode.string [m.transactionsRev, m.filter', m.addTag]
        } |> Http.fromJson bool
          |> Task.toMaybe
          |> Task.map (AddTagResponse << withDefault False)
          |> Effects.task

performRemoveTag : Model -> String -> Effects Action
performRemoveTag m t =
    Http.send Http.defaultSettings
        { verb = "PUT"
        , headers = [("Content-Type", "application/json")]
        , url = baseUrl ++ "removeTags"
        , body = Http.string <| encode 0 <| Json.Encode.list <| List.map Json.Encode.string [m.transactionsRev, m.filter', t]
        } |> Http.fromJson bool
          |> Task.toMaybe
          |> Task.map (AddTagResponse << withDefault False)
          |> Effects.task

getTransactions : Effects Action
getTransactions =
    Http.get (tuple2 (,) string (list transaction)) (baseUrl ++ "transactions")
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
