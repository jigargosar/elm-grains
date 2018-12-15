module GrainStore exposing
    ( GrainStore
    , addGrain
    , allAsList
    , decoder
    , deleteGrain
    , encoder
    , firebaseChanges
    , get
    , init
    , loadCache
    , removeGrain
    , setGrainTitle
    )

import BasicsX exposing (callWith, unwrapMaybe)
import DecodeX exposing (Encoder)
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainLookup exposing (GrainLookup)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import Return3 as R3 exposing (Return3F)


type alias GrainStore =
    GrainLookup


init =
    GrainLookup.init


allAsList =
    GrainLookup.asList


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    GrainLookup.get gid


addGrain grain lookup =
    GrainLookup.upsert grain lookup


removeGrain =
    GrainLookup.remove << Grain.id


setGrainTitle grain title lookup =
    let
        gid =
            Grain.id grain

        newLookup =
            GrainLookup.update gid (Grain.setContent title) lookup
    in
    ( get gid newLookup |> Maybe.withDefault grain, newLookup )


loadCache val gs =
    DecodeX.decode gs decoder val


deleteGrain grain lookup =
    let
        gid =
            Grain.id grain

        newLookup =
            GrainLookup.update gid (Grain.setDeleted True) lookup
    in
    ( get gid newLookup |> Maybe.withDefault grain, newLookup )


encoder : Encoder GrainStore
encoder =
    GrainLookup.encoder


decoder : Decoder GrainStore
decoder =
    GrainLookup.decoder


upsertGrain : Grain -> GrainStore -> GrainStore
upsertGrain grain =
    GrainLookup.upsert grain


cache =
    encoder >> Port.cacheGrains


firebaseChanges changes model =
    let
        handleChange { doc, type_ } =
            case type_ of
                GrainChange.Added ->
                    upsertGrain doc

                GrainChange.Modified ->
                    upsertGrain doc

                GrainChange.Removed ->
                    removeGrain doc

        newModel =
            List.foldr handleChange model changes
    in
    ( newModel, cache newModel )
