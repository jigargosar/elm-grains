module GrainStore exposing
    ( GrainStore
    , addGrainWithNewSeed
    , allAsList
    , decoder
    , deleteGrain
    , encoder
    , generator
    , get
    , setGrainTitle
    , upsertGrain
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
    { lookup : GrainLookup
    , seed : Seed
    }


initWithSeed : Seed -> GrainStore
initWithSeed seed =
    { lookup = GrainLookup.init, seed = seed }


generator : Generator GrainStore
generator =
    Random.independentSeed |> Random.map initWithSeed


allAsList =
    .lookup >> GrainLookup.asList


lookup =
    .lookup


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    lookup >> GrainLookup.get gid


setSeed seed =
    \model -> { model | seed = seed }


mapLookup : (GrainLookup -> GrainLookup) -> GrainStore -> GrainStore
mapLookup fn model =
    { model | lookup = fn model.lookup }


addGrain grain =
    mapLookup (GrainLookup.upsert grain)


addGrainWithNewSeed grain seed =
    addGrain grain >> setSeed seed


setGrainTitle grain title =
    let
        gid =
            Grain.id grain

        updateByGid fn =
            mapLookup (GrainLookup.update gid fn)
    in
    updateByGid (Grain.setContent title)


deleteGrain grain =
    let
        gid =
            Grain.id grain

        updateByGid fn =
            mapLookup (GrainLookup.update gid fn)
    in
    updateByGid (Grain.setDeleted True)


encoder : Encoder GrainStore
encoder model =
    E.object
        [ ( "lookup", GrainLookup.encoder model.lookup ) ]


decoder : Seed -> Decoder GrainStore
decoder seed =
    DecodeX.start GrainStore
        |> required "lookup" GrainLookup.decoder
        |> hardcoded seed


upsertGrain : Grain -> GrainStore -> GrainStore
upsertGrain grain =
    let
        mapper : GrainLookup -> GrainLookup
        mapper =
            GrainLookup.upsert grain
    in
    mapLookup mapper
