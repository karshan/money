module API where

import Effects     exposing (Effects)
import Http
import Json.Decode exposing (Decoder)
import Task

baseUrl : String
baseUrl = "https://karshan.me"

doPut : String -> String -> Decoder a -> (Maybe a -> action) -> Effects action
doPut endpoint data decoder cb =
    Http.send Http.defaultSettings
        { verb = "PUT"
        , headers = [("Content-Type", "application/json")]
        , url = baseUrl ++ endpoint
        , body = Http.string <| data
        } |> Http.fromJson decoder
          |> Task.toMaybe
          |> Task.map cb
          |> Effects.task

doGet : String -> Decoder a -> (Maybe a -> action) -> Effects action
doGet endpoint decoder cb =
    Http.send Http.defaultSettings
        { verb = "GET"
        , headers = []
        , body = Http.string ""
        , url = baseUrl ++ endpoint
        } |> Http.fromJson decoder
          |> Task.toMaybe
          |> Task.map cb
          |> Effects.task
