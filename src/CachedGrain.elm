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
import Cached exposing (Cached)
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


type alias Model =
    Cached Grain


type CachedGrain
    = CachedGrain Model


unwrap (CachedGrain model) =
    model


map fn =
    unwrap >> fn >> CachedGrain


decoder : Decoder CachedGrain
decoder =
    Cached.decoder Grain.decoder
        |> D.map CachedGrain


encoder : CachedGrain -> Value
encoder =
    unwrap >> Cached.encoder Grain.encoder


id : CachedGrain -> GrainId
id =
    value >> Grain.id


new : Grain -> CachedGrain
new grain =
    CachedGrain grain grain


value : CachedGrain -> Grain
value =
    unwrap >> Cached.value


setSaved : Grain -> CachedGrain -> CachedGrain
setSaved newInitial =
    map (Cached.setSaved newInitial)


isSaved : CachedGrain -> Bool
isSaved =
    unwrap >> Cached.isSaved


change : (Grain -> Grain) -> CachedGrain -> CachedGrain
change fn =
    map (Cached.change fn)
