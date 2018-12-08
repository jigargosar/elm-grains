module SystemLabel exposing
    ( SystemLabel
    , decoder
    , displayName
    , encoder
    , getMissing
    , id
    )

import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import SystemLabelId exposing (SystemLabelId)


type alias Model =
    { id : SystemLabelId
    }


type SystemLabel
    = SystemLabel Model


encoder : Encoder SystemLabel
encoder (SystemLabel model) =
    E.object
        [ ( "id", SystemLabelId.encoder model.id )
        ]


decoder : Decoder SystemLabel
decoder =
    DecodeX.start Model
        |> required "id" SystemLabelId.decoder
        |> D.map SystemLabel


unwrap : SystemLabel -> Model
unwrap (SystemLabel model) =
    model


id =
    unwrap >> .id


fromId =
    Model >> SystemLabel


getMissing : List SystemLabel -> List SystemLabel
getMissing =
    List.map id >> SystemLabelId.getMissing >> List.map fromId


displayName =
    id >> SystemLabelId.displayName
