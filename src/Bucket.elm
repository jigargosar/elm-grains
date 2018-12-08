module Bucket exposing (Bucket(..), all, decoder, displayName, encoder)

import BasicsX exposing (unwrapMaybe)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)


type Bucket
    = InBasket
    | Reference
    | NextAction
    | Someday


all =
    [ InBasket, NextAction, Reference, Someday ]


encoder : Bucket -> Value
encoder bucket =
    E.string <|
        case bucket of
            InBasket ->
                "InBasket"

            Reference ->
                "Reference"

            NextAction ->
                "NextAction"

            Someday ->
                "Someday"


decoder : Decoder Bucket
decoder =
    D.string
        |> D.map
            (\strId ->
                case strId of
                    "InBasket" ->
                        Just InBasket

                    "Reference" ->
                        Just Reference

                    "NextAction" ->
                        Just NextAction

                    "Someday" ->
                        Just Someday

                    "Trash" ->
                        Just InBasket

                    _ ->
                        Nothing
            )
        |> D.andThen (unwrapMaybe (D.fail "Invalid Bucket") D.succeed)


displayName : Bucket -> String
displayName bucket =
    case bucket of
        InBasket ->
            "InBasket"

        Reference ->
            "Reference"

        NextAction ->
            "NextAction"

        Someday ->
            "Someday"
