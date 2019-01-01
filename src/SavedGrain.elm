module SavedGrain exposing
    ( SavedGrain
    , change
    , decoder
    , discard
    , encoder
    , eqByParentId
    , id
    , idEq
    , needsPersistence
    , neverPersisted
    , new
    , setPersisted
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
    = NeverPersisted SavedModel
    | Persisted SavedModel


unwrap : SavedGrain -> SavedModel
unwrap model =
    case model of
        NeverPersisted saved ->
            saved

        Persisted saved ->
            saved


map : (SavedModel -> SavedModel) -> SavedGrain -> SavedGrain
map fn model =
    case model of
        NeverPersisted saved ->
            fn saved |> NeverPersisted

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


idEq : GrainId -> SavedGrain -> Bool
idEq gid =
    value >> Grain.idEq gid


eqByParentId : SavedGrain -> SavedGrain -> Bool
eqByParentId s1 s2 =
    parentId s1 == parentId s2


parentId : SavedGrain -> Grain.ParentId
parentId =
    value >> Grain.parentId


new : Grain -> SavedGrain
new grain =
    NeverPersisted (Saved.new grain)


value : SavedGrain -> Grain
value =
    unwrap >> Saved.value


setPersisted : Grain -> SavedGrain -> SavedGrain
setPersisted newInitial =
    unwrap >> Saved.setSaved newInitial >> Persisted


neverPersisted : SavedGrain -> Bool
neverPersisted model =
    case model of
        NeverPersisted saved ->
            True

        Persisted saved ->
            False


needsPersistence : SavedGrain -> Bool
needsPersistence model =
    case model of
        NeverPersisted saved ->
            True

        Persisted saved ->
            Saved.isSaved saved |> not


change : (Grain -> Grain) -> SavedGrain -> SavedGrain
change fn =
    map (Saved.change fn)


discard =
    map Saved.discard
