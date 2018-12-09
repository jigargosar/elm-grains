module GrainId exposing (GrainId, asString, generator)

import Random exposing (Generator)
import RandomId


type alias Model =
    String


type GrainId
    = GrainId Model


unwrap (GrainId model) =
    model


asString =
    unwrap


generator : Generator GrainId
generator =
    RandomId.generator "GrainId_"
        |> Random.map GrainId
