module CachedGrain exposing
    ( CachedGrain
    , change
    , decoder
    , encoder
    , id
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


type CachedGrain
    = CachedGrain Grain Grain


decoder : Decoder CachedGrain
decoder =
    D.map2 CachedGrain
        (D.field "initial" Grain.decoder)
        (D.field "latest" Grain.decoder)


encoder : CachedGrain -> Value
encoder (CachedGrain initial latest) =
    E.object
        [ ( "initial", Grain.encoder initial )
        , ( "latest", Grain.encoder latest )
        ]


id : CachedGrain -> GrainId
id (CachedGrain _ latest) =
    Grain.id latest


new : Grain -> CachedGrain
new grain =
    CachedGrain grain grain


value : CachedGrain -> Grain
value (CachedGrain initial latest) =
    latest


setSaved : Grain -> CachedGrain -> CachedGrain
setSaved newInitial (CachedGrain _ latest) =
    CachedGrain newInitial latest


isSaved : CachedGrain -> Bool
isSaved (CachedGrain initial latest) =
    initial == latest


change : (Grain -> Grain) -> CachedGrain -> CachedGrain
change fn (CachedGrain initial latest) =
    let
        newLatest : Grain
        newLatest =
            fn latest

        newInitial : Grain
        newInitial =
            if initial == newLatest then
                newLatest

            else
                initial
    in
    CachedGrain newInitial newLatest
