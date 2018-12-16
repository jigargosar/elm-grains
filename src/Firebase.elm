module Firebase exposing
    ( Msg(..)
    , decoder
    , persistGrains
    , persistNewGrain
    , persistRemovedGrain
    , persistUpdatedGrain
    , signIn
    , signOut
    )

import DecodeX
import FireUser exposing (FireUser)
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as E
import Port
import Random exposing (Generator)


type Msg
    = AuthUser FireUser
    | AuthUserNone
    | GrainChangesGenerator (Generator (List GrainChange))
    | InvalidMsg String


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
            DecodeX.start GrainChangesGenerator
                |> requiredAt [ "payload", "changes" ] GrainChange.listDecoder

        _ ->
            D.succeed <| InvalidMsg msgString


persistGrains =
    Port.persistGrains


persistNewGrain =
    Port.persistNewGrain << Grain.encoder


persistUpdatedGrain =
    Port.persistUpdatedGrain << Grain.encoder


persistRemovedGrain =
    Port.persistRemovedGrain << Grain.encoder


signIn =
    Port.signIn


signOut =
    Port.signOut
