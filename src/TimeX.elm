module TimeX exposing (Millis, now, nowMilli, posixDecoder, posixEncoder)

import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Task exposing (Task)
import Time exposing (Posix)


type alias Millis =
    Int


nowMilli : Task x Millis
nowMilli =
    Task.map Time.posixToMillis Time.now


now : (Millis -> msg) -> Cmd msg
now toMsg =
    Task.perform toMsg nowMilli


posixEncoder : Encoder Posix
posixEncoder posix =
    E.int (Time.posixToMillis posix)


posixDecoder : Decoder Posix
posixDecoder =
    D.int |> D.map Time.millisToPosix
