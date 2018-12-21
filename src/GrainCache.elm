module GrainCache exposing (GrainCache, decoder, empty)

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
import Saved exposing (Saved)


type alias SavedGrain =
    Saved Grain


savedGrainId : SavedGrain -> GrainId
savedGrainId =
    Saved.value >> Grain.id


savedGrainDecoder : Decoder SavedGrain
savedGrainDecoder =
    let
        decoderWithValue : Grain -> Decoder SavedGrain
        decoderWithValue value =
            let
                saved : SavedGrain
                saved =
                    Saved.new value
            in
            D.maybe (D.field "initial" Grain.decoder)
                |> D.map (Maybe.unwrap identity Saved.setSaved >> callWith saved)
    in
    D.field "value" Grain.decoder
        |> D.andThen decoderWithValue


savedGrainEncoder : SavedGrain -> Value
savedGrainEncoder model =
    if Saved.saved model then
        E.object
            [ ( "value", Grain.encoder (Saved.value model) )
            ]

    else
        E.object
            [ ( "initial", Grain.encoder (Saved.discard model |> Saved.value) )
            , ( "value", Grain.encoder (Saved.value model) )
            ]


type alias GrainCache =
    GrainIdLookup (Saved Grain)


empty : GrainCache
empty =
    GrainIdLookup.empty


decoder : Decoder GrainCache
decoder =
    D.list savedGrainDecoder
        |> D.map (GrainIdLookup.fromList savedGrainId)
