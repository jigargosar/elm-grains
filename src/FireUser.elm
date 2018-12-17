module FireUser exposing (FireUser, decoder)

import DecodeX
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


type alias Model =
    { uid : String
    , displayName : String
    , email : String
    }


type FireUser
    = FireUser Model


unwrap (FireUser model) =
    model


map fn =
    unwrap >> fn >> FireUser


decoder : Decoder FireUser
decoder =
    DecodeX.start Model
        |> required "uid" D.string
        |> required "displayName" D.string
        |> required "email" D.string
        |> D.map FireUser
