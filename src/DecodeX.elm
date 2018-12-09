module DecodeX exposing (DecodeResult, Encoder, decode, resultToReturn, start)

import BasicsX exposing (callWith)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Port
import Result.Extra as Result
import Return exposing (..)


type alias Encoder a =
    a -> Value


start =
    D.succeed


type alias DecodeResult a =
    Result D.Error a


resultToReturn : a -> DecodeResult a -> Return msg a
resultToReturn a =
    Result.mapBoth
        (D.errorToString >> Port.error >> return a)
        singleton
        >> Result.merge


decode default decoder =
    D.decodeValue decoder
        >> resultToReturn default
