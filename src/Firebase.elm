module Firebase exposing
    ( decode
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


type Msg
    = AuthUser FireUser
    | AuthUserNone
    | GrainChanges (Generator (List GrainChange))
    | InvalidMsg String


type alias Config msg =
    { error : String -> msg
    , authUser : FireUser -> msg
    , authUserNone : () -> msg
    , grainChanges : Generator (List GrainChange) -> msg
    }


decode : Config msg -> Value -> msg
decode config val =
    case D.decodeValue decoder val of
        Err error ->
            config.error (D.errorToString error)

        Ok fireMsg ->
            case fireMsg of
                InvalidMsg unknown ->
                    config.error ("Firebase : Invalid Msg Received: " ++ unknown)

                AuthUser user ->
                    config.authUser user

                AuthUserNone ->
                    config.authUserNone ()

                GrainChanges changes ->
                    config.grainChanges changes


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
