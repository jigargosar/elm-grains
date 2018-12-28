module Saved exposing
    ( Saved
    , change
    , decoder
    , encoder
    , isSaved
    , new
    , setSaved
    , value
    )

import BasicsX exposing (callWith, eqs, flip)
import Compare
import DecodeX exposing (Encoder)
import Grain exposing (Grain)
import GrainId exposing (GrainId(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required, resolve)
import Json.Encode as E exposing (Value)
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Time exposing (Posix)
import TimeX


type Saved a
    = Saved a a


type alias SavedDecoder a =
    Decoder (Saved a)


decoder : Decoder a -> SavedDecoder a
decoder valDecoder =
    D.map2 Saved
        (D.field "initial" valDecoder)
        (D.field "latest" valDecoder)


encoder : (a -> Value) -> Saved a -> Value
encoder valEncoder (Saved initial latest) =
    E.object
        [ ( "initial", valEncoder initial )
        , ( "latest", valEncoder latest )
        ]


new : a -> Saved a
new val =
    Saved val val


value : Saved a -> a
value (Saved initial latest) =
    latest


setSaved : a -> Saved a -> Saved a
setSaved newInitial (Saved _ latest) =
    Saved newInitial latest


isSaved : Saved a -> Bool
isSaved (Saved initial latest) =
    initial == latest


change : (a -> a) -> Saved a -> Saved a
change fn (Saved initial latest) =
    let
        newLatest : a
        newLatest =
            fn latest

        newInitial : a
        newInitial =
            if initial == newLatest then
                newLatest

            else
                initial
    in
    Saved newInitial newLatest
