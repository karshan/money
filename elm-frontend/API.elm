module API where

import Effects     exposing (Effects)
import Http
import Json.Decode exposing (Decoder)
import Task


baseUrl : String
baseUrl = "https://karshan.me"


send : String -> List (String, String) -> String -> String -> Decoder a -> (Maybe a -> action) -> Effects action
send verb headers endpoint data decoder cb =
    Http.send Http.defaultSettings
        { verb = verb
        , headers = headers
        , url = baseUrl ++ endpoint
        , body = Http.string <| data
        } |> Http.fromJson decoder
          |> Task.toMaybe
          |> Task.map cb
          |> Effects.task


doPut : String -> String -> Decoder a -> (Maybe a -> action) -> Effects action
doPut =
    send "PUT" [("Content-Type", "application/json")]


doGet : String -> Decoder a -> (Maybe a -> action) -> Effects action
doGet endpoint decoder cb =
    send "GET" [] endpoint "" decoder cb
