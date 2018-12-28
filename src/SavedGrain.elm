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
import Saved exposing (Saved)
import Time exposing (Posix)
import TimeX


type alias SavedModel =
    Saved Grain


type SavedGrain
    = UnPersisted SavedModel
    | Persisted SavedModel


unwrap : SavedGrain -> SavedModel
unwrap model =
    case model of
        UnPersisted saved ->
            saved

        Persisted saved ->
            saved


map : (SavedModel -> SavedModel) -> SavedGrain -> SavedGrain
map fn model =
    case model of
        UnPersisted saved ->
            fn saved |> UnPersisted

        Persisted saved ->
            fn saved |> Persisted


decoder : Decoder SavedGrain
decoder =
    let
        savedDecoder : Decoder SavedModel
        savedDecoder =
            Saved.decoder Grain.decoder
    in
    D.oneOf [ savedDecoder ]
        |> D.map Persisted


encoder : SavedGrain -> Value
encoder =
    unwrap >> Saved.encoder Grain.encoder


id : SavedGrain -> GrainId
id =
    value >> Grain.id


new : Grain -> SavedGrain
new grain =
    UnPersisted (Saved.new grain)


value : SavedGrain -> Grain
value =
    unwrap >> Saved.value


setSaved : Grain -> SavedGrain -> SavedGrain
setSaved newInitial =
    map (Saved.setSaved newInitial)


isSaved : SavedGrain -> Bool
isSaved =
    unwrap >> Saved.isSaved


change : (Grain -> Grain) -> SavedGrain -> SavedGrain
change fn =
    map (Saved.change fn)
