module Cached exposing
    ( Cached
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


type Cached a
    = Cached a a


type alias CachedDecoder a =
    Decoder (Cached a)


decoder : Decoder a -> CachedDecoder a
decoder valDecoder =
    D.map2 Cached
        (D.field "initial" valDecoder)
        (D.field "latest" valDecoder)


encoder : (a -> Value) -> Cached a -> Value
encoder valEncoder (Cached initial latest) =
    E.object
        [ ( "initial", valEncoder initial )
        , ( "latest", valEncoder latest )
        ]


new : a -> Cached a
new val =
    Cached val val


value : Cached a -> a
value (Cached initial latest) =
    latest


setSaved : a -> Cached a -> Cached a
setSaved newInitial (Cached _ latest) =
    Cached newInitial latest


isSaved : Cached a -> Bool
isSaved (Cached initial latest) =
    initial == latest


change : (a -> a) -> Cached a -> Cached a
change fn (Cached initial latest) =
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
    Cached newInitial newLatest
