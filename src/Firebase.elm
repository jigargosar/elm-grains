module Firebase exposing
    ( AuthState(..)
    , Msg(..)
    , decodeInbound
    , initialAuthState
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
import Json.Encode as E exposing (Value)
import Port
import Random exposing (Generator)


type AuthState
    = AuthStateLoading
    | AuthStateUser FireUser
    | AuthStateNoUser


initialAuthState =
    AuthStateLoading


type Msg
    = AuthStateChanged AuthState
    | GrainChanges (Generator (List GrainChange))
    | Error String


decodeInbound : Value -> Msg
decodeInbound val =
    case D.decodeValue decoder val of
        Err error ->
            Error ("Decoding Error : " ++ D.errorToString error)

        Ok fireMsg ->
            fireMsg


decoder : Decoder Msg
decoder =
    D.field "msg" D.string
        |> D.andThen decoderWithMsg


decoderWithMsg msgString =
    case msgString of
        "UserLoggedIn" ->
            DecodeX.start (AuthStateChanged << AuthStateUser)
                |> requiredAt [ "payload", "user" ] FireUser.decoder

        "UserNotLoggedIn" ->
            D.succeed (AuthStateChanged AuthStateNoUser)

        "GrainChanges" ->
            DecodeX.start GrainChanges
                |> requiredAt [ "payload", "changes" ] GrainChange.listDecoder

        _ ->
            D.succeed <| Error ("UnknownMsg: " ++ msgString)


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
