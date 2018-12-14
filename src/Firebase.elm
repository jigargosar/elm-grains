module Firebase exposing
    ( Msg(..)
    , decoder
    , persistGrains
    , persistNewGrain
    , signIn
    , signOut
    , updateGrain
    )

import DecodeX
import FireUser exposing (FireUser)
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as E
import Port


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


persistGrains =
    Port.persistGrains


persistNewGrain grain =
    Port.persistGrain (Grain.encoder grain)


updateGrain grain =
    Port.persistUpdateGrain (Grain.encoder grain)


signIn =
    Port.signIn


signOut =
    Port.signOut
