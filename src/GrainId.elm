module GrainId exposing
    ( GrainId
    , decoder
    , encoder
    , fromString
    , generator
    , root
    , toDomIdWithPrefix
    , toString
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


root : GrainId
root =
    prefix
        ++ "__ROOT__"
        |> GrainId


prefix =
    "GrainId_"


generator : Generator GrainId
generator =
    RandomId.generator prefix
        |> Random.map GrainId


encoder : Encoder GrainId
encoder (GrainId model) =
    E.string model


decoder : Decoder GrainId
decoder =
    D.string |> D.map GrainId


fromString : String -> Maybe GrainId
fromString string =
    if GrainId string == root || RandomId.isValidWithPrefix prefix string then
        Just <| GrainId string

    else
        Nothing


unwrap (GrainId model) =
    model


toString =
    unwrap


toDomIdWithPrefix prefix_ =
    unwrap >> (++) prefix_
