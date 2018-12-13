module Fire2Elm exposing (decoder)

import DecodeX
import FireUser
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Json.Encode as E
import Msg exposing (Msg)


decoder : Decoder Msg
decoder =
    D.field "msg" D.string
        |> D.andThen decoderWithMsg


decoderWithMsg msg =
    case msg of
        "UserLoggedIn" ->
            DecodeX.start Msg.AuthUser
                |> requiredAt [ "payload", "user" ] FireUser.decoder

        "UserNotLoggedIn" ->
            D.succeed Msg.AuthUserNone

        _ ->
            D.fail "Invalid fire2Elm msg"
