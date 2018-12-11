module GrainId exposing
    ( GrainId
    , decoder
    , encoder
    , generator
    , toDomIdWithPrefix
    )

import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random exposing (Generator)
import RandomId
import String.Extra as String


type alias Model =
    String


type GrainId
    = GrainId Model


generator : Generator GrainId
generator =
    RandomId.generator "GrainId_"
        |> Random.map GrainId


encoder : Encoder GrainId
encoder (GrainId model) =
    E.string model


decoder : Decoder GrainId
decoder =
    D.string |> D.map GrainId


unwrap (GrainId model) =
    model


asString =
    unwrap


toDomIdWithPrefix prefix =
    unwrap >> (++) prefix
