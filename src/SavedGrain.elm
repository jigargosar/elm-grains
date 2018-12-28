module SavedGrain exposing
    ( SavedGrain
    , change
    , decoder
    , encoder
    , id
    , new
    , saved
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


type SavedGrain
    = SavedGrain Grain Grain


decoder : Decoder SavedGrain
decoder =
    D.map2 SavedGrain
        (D.field "initial" Grain.decoder)
        (D.field "latest" Grain.decoder)


encoder : SavedGrain -> Value
encoder (SavedGrain initial latest) =
    E.object
        [ ( "initial", Grain.encoder initial )
        , ( "latest", Grain.encoder latest )
        ]


id : SavedGrain -> GrainId
id (SavedGrain _ latest) =
    Grain.id latest


new : Grain -> SavedGrain
new grain =
    SavedGrain grain grain


value : SavedGrain -> Grain
value (SavedGrain initial latest) =
    latest


setSaved : Grain -> SavedGrain -> SavedGrain
setSaved newInitial (SavedGrain _ latest) =
    SavedGrain newInitial latest


save : SavedGrain -> SavedGrain
save (SavedGrain _ latest) =
    SavedGrain latest latest


saved : SavedGrain -> Bool
saved (SavedGrain initial latest) =
    initial == latest


change : (Grain -> Grain) -> SavedGrain -> SavedGrain
change fn (SavedGrain initial latest) =
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
    SavedGrain newInitial newLatest
