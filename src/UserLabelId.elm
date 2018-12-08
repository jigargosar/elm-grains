module UserLabelId exposing
    ( UserLabelId
    , generator, asString
    , encoder, decoder
    )

{-| UserLabel Identifier

@docs UserLabelId
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


type UserLabelId
    = UserLabelId String


unwrap : UserLabelId -> Model
unwrap (UserLabelId model) =
    model


asString =
    unwrap


generator : Generator UserLabelId
generator =
    RandomId.stringIdGenerator
        |> Random.map ((\id -> "userLabel--" ++ id) >> UserLabelId)


encoder : Encoder UserLabelId
encoder (UserLabelId model) =
    E.string model


decoder : Decoder UserLabelId
decoder =
    D.string |> D.map UserLabelId
