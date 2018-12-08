module Label exposing
    ( Label
    , addMissingSystemLabels
    , decoder
    , displayName
    , encoder
    , hasId
    , id
    )

import BasicsX exposing (eqs)
import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import LabelId exposing (LabelId)
import SystemLabel exposing (SystemLabel)
import UserLabel exposing (UserLabel)


type Label
    = System SystemLabel
    | User UserLabel


decoder : Decoder Label
decoder =
    D.oneOf
        [ SystemLabel.decoder |> D.map System
        , UserLabel.decoder |> D.map User
        ]


encoder : Encoder Label
encoder model =
    case model of
        System labelModel ->
            SystemLabel.encoder labelModel

        User labelModel ->
            UserLabel.encoder labelModel


id model =
    case model of
        System label ->
            SystemLabel.id label |> LabelId.System

        User label ->
            UserLabel.id label |> LabelId.User


hasId id_ =
    id >> eqs id_


isSystem label =
    case label of
        System _ ->
            True

        _ ->
            False


getSystemLabels =
    List.filterMap
        (\label ->
            case label of
                System s ->
                    Just s

                _ ->
                    Nothing
        )


addMissingSystemLabels : List Label -> List Label
addMissingSystemLabels labels =
    labels
        |> getSystemLabels
        |> SystemLabel.getMissing
        |> List.map System
        |> (++) labels


displayName model =
    case model of
        System label ->
            SystemLabel.displayName label

        User label ->
            UserLabel.displayName label
