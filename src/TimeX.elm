module TimeX exposing (posixDecoder, posixEncoder)

import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Task exposing (Task)
import Time exposing (Posix)


posixEncoder : Encoder Posix
posixEncoder posix =
    E.int (Time.posixToMillis posix)


posixDecoder : Decoder Posix
posixDecoder =
    D.int |> D.map Time.millisToPosix
