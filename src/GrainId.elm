module GrainId exposing
    ( GrainId
    , generator, asString
    , encoder, decoder
    )

{-| Grain Identifier

@docs GrainId
@docs generator, asString
@docs encoder, decoder

-}

import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random exposing (Generator)
import RandomId


type alias Model =
    String


type GrainId
    = GrainId String


unwrap : GrainId -> Model
unwrap (GrainId model) =
    model


asString =
    unwrap


generator : Generator GrainId
generator =
    RandomId.stringIdGenerator
        |> Random.map ((\id -> "grain--" ++ id) >> GrainId)


encoder : Encoder GrainId
encoder (GrainId model) =
    E.string model


decoder : Decoder GrainId
decoder =
    D.string |> D.map GrainId
