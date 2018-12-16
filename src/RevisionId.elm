module RevisionId exposing (RevisionId, decoder, encoder, generator)

import Json.Decode as D
import Json.Encode as E
import Random
import RandomId


type alias Model =
    String


type RevisionId
    = RevisionId Model


unwrap (RevisionId model) =
    model


map fn =
    unwrap >> fn >> RevisionId


prefix =
    "Rev__"


generator =
    RandomId.generator prefix
        |> Random.map RevisionId


encoder =
    unwrap >> RandomId.encoder


decoder =
    RandomId.decoder prefix |> D.map RevisionId
