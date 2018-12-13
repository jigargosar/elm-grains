module Fire2Elm exposing (decoder)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Msg exposing (Msg)


decoder : Decoder Msg
decoder =
    D.field "msg" D.string
        |> D.andThen decoderWithMsg


decoderWithMsg msg =
    case msg of
        "UserLoggedIn" ->
            D.succeed Msg.UserLoggedIn

        "UserNotLoggedIn" ->
            D.succeed Msg.UserNotLoggedIn

        _ ->
            D.fail "Invalid fire2Elm msg"
