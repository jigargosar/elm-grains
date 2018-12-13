module Fire2Elm exposing (decoder)

import DecodeX
import FireUser
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
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

        "GrainChanges" ->
            DecodeX.start Msg.grainFirestoreChanges
                |> requiredAt [ "payload", "changes" ] GrainChange.listDecoder

        _ ->
            D.fail "Invalid fire2Elm msg"
