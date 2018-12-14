module Firebase exposing (Msg(..), decoder)

import DecodeX
import FireUser exposing (FireUser)
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as E


type Msg
    = AuthUser FireUser
    | AuthUserNone
    | GrainChanges (List GrainChange)
    | UnknownMsg String


decoder : Decoder Msg
decoder =
    D.field "msg" D.string
        |> D.andThen decoderWithMsg


decoderWithMsg msgString =
    case msgString of
        "UserLoggedIn" ->
            DecodeX.start AuthUser
                |> requiredAt [ "payload", "user" ] FireUser.decoder

        "UserNotLoggedIn" ->
            D.succeed AuthUserNone

        "GrainChanges" ->
            DecodeX.start GrainChanges
                |> requiredAt [ "payload", "changes" ] GrainChange.listDecoder

        _ ->
            D.succeed <| UnknownMsg msgString
