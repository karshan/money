module Main where

import List exposing (indexedMap, take, append, length, concat)
import List.Util exposing (index, modifyAt)

import Signal exposing (forwardTo)
import StartApp
import Effects
import Task

import Html exposing (div, span, strong, text)

import Ui.Container
import Ui.Button
import Ui.Input
import Ui.App
import Ui

-- sqa is short for Secret-Question-Answer
type alias Model =
  { app : Ui.App.Model
  , username : Ui.Input.Model
  , password : Ui.Input.Model
  , sqaPairs : List (Ui.Input.Model, Ui.Input.Model)
  }

init : Model
init =
  let
    initPassword = Ui.Input.init "" "password"
  in
    { app = Ui.App.init "money"
    , username = Ui.Input.init "" "username"
    , password = { initPassword | kind = "password" }
    , sqaPairs = []
    }

type Action
  = App Ui.App.Action
  | Username Ui.Input.Action
  | Password Ui.Input.Action
  | SecretQuestion Int Ui.Input.Action
  | SecretAnswer Int Ui.Input.Action
  | AddSQAPair
  | RemoveSQAPair
  | NoOp

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    App act ->
      let (a, e) = Ui.App.update act model.app
      in  ({ model | app = a }, Effects.map App e)

    Username act ->
      let (a, e) = Ui.Input.update act model.username
      in  ({ model | username = a }, Effects.none)

    Password act ->
      let (a, e) = Ui.Input.update act model.password
      in  ({ model | password = a }, Effects.none)

    SecretQuestion i act ->
      ({ model | sqaPairs = 
          modifyAt i 
            (\(q, a) -> (fst <| Ui.Input.update act q, a))
            model.sqaPairs }
      , Effects.none)

    SecretAnswer i act ->
      ({ model | sqaPairs = 
          modifyAt i 
            (\(q, a) -> (q, fst <| Ui.Input.update act a))
            model.sqaPairs }
      , Effects.none)

    AddSQAPair ->
      ({ model | sqaPairs =
          append 
            model.sqaPairs 
            [( Ui.Input.init "" "question"
            , Ui.Input.init "" "answer"
            )] }
      , Effects.none)

    RemoveSQAPair ->
      ({ model | sqaPairs =
          take (length model.sqaPairs - 1) model.sqaPairs }
      , Effects.none)

    NoOp ->
      (model, Effects.none)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  Ui.App.view (forwardTo address App) model.app
    [ Ui.Container.column [] <|
      concat
        [ [ Ui.Input.view (forwardTo address Username) model.username
          , Ui.Input.view (forwardTo address Password) model.password 
          ]
        , indexedMap (\i (q, a) ->
            Ui.Container.row []
              [ Ui.Input.view (forwardTo address (SecretQuestion i)) q
              , Ui.Input.view (forwardTo address (SecretAnswer i)) a
              ]) model.sqaPairs
        , [ Ui.Button.primary "Add" address AddSQAPair
          , Ui.Button.primary "Remove" address RemoveSQAPair
          ]
        ]
    ]

app =
  StartApp.start { init = (init, Effects.none)
                 , update = update
                 , view = view
                 , inputs = []
                 }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
