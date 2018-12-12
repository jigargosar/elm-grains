module Revision exposing (Revision, decoder, encoder, init)

import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


type alias Model =
    { version : Int
    }


type Revision
    = Revision Model


init =
    Revision { version = 0 }


encoder : Encoder Revision
encoder (Revision model) =
    E.object
        [ ( "version", E.int model.version )
        ]


decoder : Decoder Revision
decoder =
    DecodeX.start Model
        |> required "revision" D.int
        |> D.map Revision


unwrap (Revision model) =
    model


map fn =
    unwrap >> fn >> Revision
