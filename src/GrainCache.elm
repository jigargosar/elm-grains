module GrainCache exposing (GrainCache, empty)

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


type alias GrainCache =
    GrainIdLookup (Saved Grain)


empty : GrainCache
empty =
    GrainIdLookup.empty
