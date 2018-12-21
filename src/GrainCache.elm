module GrainCache exposing
    ( GrainCache
    , decoder
    , empty
    , encoder
    , remove
    , setSaved
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


update fn gid model =
    if GrainIdLookup.member gid model then
        Result.Ok <| GrainIdLookup.updateIfExists gid fn

    else
        Result.Err "GrainNotFound"
