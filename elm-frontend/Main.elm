import API             exposing (doGet, doPut)
import Decoders
import Effects         exposing (Effects, Never)
import Encoders
import Filter          exposing (parseFilter)
import Html            exposing (Html)
import Json.Decode     exposing (Decoder, tuple2, object1, object4, list, string, int, bool, (:=))
import Json.Encode     exposing (encode, Value)
import List            exposing (filter, foldr, map, sortBy, reverse, member, length)
import Maybe           exposing (withDefault)
import Model           exposing (Model, Transaction, Action (..), initModel)
import Signal          exposing (Address, message)
import StartApp        exposing (start)
import Task            exposing (Task)
import Util            exposing (Either, sha256)
import View            exposing (renderTransactions, mkTable, view)

app : StartApp.App Model
app =
    start { init = init, view = view, update = update, inputs = [] }

main : Signal Html
main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

init : (Model, Effects Action)
init = (initModel, getTransactions)

hashTransactions : List Transaction -> String
hashTransactions = sha256 << encode 0 << Encoders.transactions

update : Action -> Model -> (Model, Effects Action)
update action m = case action of
    (LoadTransactions (rev, ts)) -> ({ m | transactions = ts, transactionsRev = rev }, Effects.none)
    (LoadCategorizers cats) -> ({ m | categorizers = map (\(a, b) -> (parseFilter a, b)) cats }, Effects.none)
    (Filter s) -> ({ m | filter' = s }, Effects.none)
    FilterEnter -> ({ m | filterExpr = parseFilter m.filter' }, Effects.none)
    (AddTag s) -> ({ m | addTag = s }, Effects.none)
    PerformAddTag -> (m, performAddTag m)
    AddTagResponse b -> if b then (m, getTransactions) else ({ m | error = True }, Effects.none)
    RemoveTag s -> (m, performRemoveTag m s)
    NoOp -> (m, Effects.none)

performAddTag : Model -> Effects Action
performAddTag m =
    let data = encode 0 <| Json.Encode.list <|
            [Json.Encode.string m.filter', Json.Encode.list [Json.Encode.string m.addTag]]
    in doPut "addCategorizer" data (list string) (AddTagResponse << always True) -- TODO NO

performRemoveTag : Model -> String -> Effects Action
performRemoveTag m tag =
    let data = encode 0 <| Json.Encode.list <| map Json.Encode.string [m.transactionsRev, m.filter', tag]
    in doPut "removeTags" data bool (AddTagResponse << withDefault False)

getTransactions : Effects Action
getTransactions =
    Effects.batch
        [ doGet "transactions"
                (tuple2 (,) string Decoders.transactions)
                (LoadTransactions << withDefault ("", []))
        , doGet "categorizers"
                (list (tuple2 (,) string (list string)))
                (LoadCategorizers << withDefault [])
        ]
