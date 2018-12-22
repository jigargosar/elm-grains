module GrainCache exposing
    ( GrainCache
    , batchUpdate
    , decoder
    , empty
    , encoder
    , remove
    , setSaved
    , updateWithGrainMsg
    )

import ActorId exposing (ActorId)
import BasicsX exposing (callWith, ifElse, unwrapMaybe)
import DecodeX exposing (Encoder)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainDict exposing (GrainDict)
import GrainId exposing (GrainId)
import GrainIdLookup exposing (GrainIdLookup)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import RandomX
import Return exposing (Return)
import Return3 as R3 exposing (Return3F)
import SavedGrain exposing (SavedGrain)
import Time exposing (Posix)


type alias GrainCache =
    GrainIdLookup SavedGrain


decoder : Decoder GrainCache
decoder =
    D.list SavedGrain.decoder
        |> D.map (GrainIdLookup.fromList SavedGrain.id)


encoder : GrainCache -> Value
encoder =
    GrainIdLookup.toList >> E.list SavedGrain.encoder


empty : GrainCache
empty =
    GrainIdLookup.empty


setSaved : Grain -> GrainCache -> GrainCache
setSaved grain =
    GrainIdLookup.update (Grain.id grain)
        (Maybe.map (SavedGrain.setSaved grain)
            >> Maybe.orElseLazy (\_ -> Just <| SavedGrain.new grain)
        )


remove grain =
    GrainIdLookup.remove (Grain.id grain)


type alias UpdateResult =
    Result String GrainCache


update :
    (Grain -> Grain)
    -> GrainId
    -> GrainCache
    -> UpdateResult
update changeFn gid model =
    if GrainIdLookup.member gid model then
        let
            updateFn =
                SavedGrain.change changeFn
        in
        Result.Ok <| GrainIdLookup.updateIfExists gid updateFn model

    else
        Result.Err "GrainNotFound"


batchUpdate : List ( Grain -> Grain, GrainId ) -> GrainCache -> UpdateResult
batchUpdate list model =
    let
        reducer ( changeFn, gid ) =
            Result.andThen (update changeFn gid)
    in
    List.foldl reducer (Result.Ok model) list


updateWithGrainMsg :
    Posix
    -> Grain.Update
    -> GrainId
    -> GrainCache
    -> UpdateResult
updateWithGrainMsg now grainMsg gid model =
    update (Grain.update now grainMsg) gid model
