module LabelId exposing
    ( LabelId(..)
    , incomplete
    , labelIdsDecoder
    , labelIdsEncoder
    , toggle
    , trash
    )

import BasicsX exposing (..)
import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import SystemLabelId exposing (SystemLabelId)
import UserLabelId exposing (UserLabelId)


type LabelId
    = System SystemLabelId
    | User UserLabelId


incomplete =
    System SystemLabelId.Todo


trash =
    System SystemLabelId.Trash


decoder : Decoder LabelId
decoder =
    D.oneOf [ SystemLabelId.decoder |> D.map System, UserLabelId.decoder |> D.map User ]


encoder : Encoder LabelId
encoder labelId =
    case labelId of
        System id ->
            SystemLabelId.encoder id

        User id ->
            UserLabelId.encoder id


labelIdsEncoder : List LabelId -> Value
labelIdsEncoder =
    E.list encoder


labelIdsDecoder : Decoder (List LabelId)
labelIdsDecoder =
    D.list decoder


toggle labelId =
    listToggleMember labelId
