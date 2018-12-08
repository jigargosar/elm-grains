module DecodeX exposing (DecodeResult, Encoder, decodeString, resultToReturn, start)

import BasicsX exposing (callWith)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Port
import Result.Extra as Result
import Return exposing (..)



--andMap : Decoder a -> Decoder (a -> b) -> Decoder b
--andMap =
--    D.map2 callOn
--
--
--required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
--required name =
--    andMap << D.field name


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


decodeString default decoder string =
    D.decodeString decoder string
        |> resultToReturn default
