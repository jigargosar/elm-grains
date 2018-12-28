module SavedGrain exposing
    ( SavedGrain
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
import Json.Decode.Pipeline
    exposing
        ( custom
        , hardcoded
        , optional
        , required
        , resolve
        )
import Json.Encode as E exposing (Value)
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Time exposing (Posix)
import TimeX


type alias Model =
    Cached Grain


type SavedGrain
    = SavedGrain Model


unwrap : SavedGrain -> Model
unwrap (SavedGrain model) =
    model


map : (Model -> Model) -> SavedGrain -> SavedGrain
map fn =
    unwrap >> fn >> SavedGrain


decoder : Decoder SavedGrain
decoder =
    Cached.decoder Grain.decoder
        |> D.map SavedGrain


encoder : SavedGrain -> Value
encoder =
    unwrap >> Cached.encoder Grain.encoder


id : SavedGrain -> GrainId
id =
    value >> Grain.id


new : Grain -> SavedGrain
new grain =
    SavedGrain (Cached.new grain)


value : SavedGrain -> Grain
value =
    unwrap >> Cached.value


setSaved : Grain -> SavedGrain -> SavedGrain
setSaved newInitial =
    map (Cached.setSaved newInitial)


isSaved : SavedGrain -> Bool
isSaved =
    unwrap >> Cached.isSaved


change : (Grain -> Grain) -> SavedGrain -> SavedGrain
change fn =
    map (Cached.change fn)
