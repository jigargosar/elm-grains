module GrainId exposing
    ( GrainId
    , generator
    , toDomIdWithPrefix
    )

import Random exposing (Generator)
import RandomId
import String.Extra as String


type alias Model =
    String


type GrainId
    = GrainId Model


unwrap (GrainId model) =
    model


asString =
    unwrap


toDomIdWithPrefix prefix =
    unwrap >> (++) prefix


generator : Generator GrainId
generator =
    RandomId.generator "GrainId_"
        |> Random.map GrainId
