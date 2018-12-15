module ActorId exposing
    ( ActorId
    , decoder
    , encoder
    , fromString
    , generator
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


type ActorId
    = ActorId Model


prefix =
    "ActorId_"


generator : Generator ActorId
generator =
    RandomId.generator "ActorId_"
        |> Random.map ActorId


encoder : Encoder ActorId
encoder (ActorId model) =
    E.string model


decoder : Decoder ActorId
decoder =
    D.string |> D.map ActorId


fromString : String -> Maybe ActorId
fromString string =
    if RandomId.isValidWithPrefix prefix string then
        Just <| ActorId string

    else
        Nothing


unwrap (ActorId model) =
    model


toString =
    unwrap
