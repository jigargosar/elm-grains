module GrainHistoryStore exposing
    ( GrainHistoryStore
    , generator
    , get
    )

import DecodeX exposing (Encoder)
import Dict exposing (Dict)
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import Return3 as R3 exposing (Return3F)


type alias GrainHistory =
    List { grain : Grain }


type alias GrainHistoryStore =
    { lookup : Dict String GrainHistory
    , seed : Seed
    }


initWithSeed seed =
    { lookup = Dict.empty, seed = seed }


generator : Generator GrainHistoryStore
generator =
    Random.independentSeed |> Random.map initWithSeed


get gid =
    .lookup >> Dict.get (GrainId.toString gid)


setSeed seed =
    \model -> { model | seed = seed }


cache : GrainHistoryStore -> Cmd msg
cache =
    encoder >> Port.cacheGrains


encoder : Encoder GrainHistoryStore
encoder model =
    E.object
        [ ( "lookup", E.dict identity Grain.encoder model.lookup ) ]


decoder : Seed -> Decoder GrainHistoryStore
decoder seed =
    DecodeX.start GrainHistoryStore
        |> required "lookup" (D.dict Grain.decoder)
        |> hardcoded seed
