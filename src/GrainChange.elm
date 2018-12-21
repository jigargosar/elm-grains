module GrainChange exposing
    ( ChangeType(..)
    , GrainChange
    , grain
    , listDecoder
    , type_
    )

import DecodeX
import Grain exposing (Grain)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E
import List.Extra as List
import Random exposing (Generator)
import RandomX


type ChangeType
    = Added
    | Modified
    | Removed


changeTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "added" ->
                        D.succeed Added

                    "modified" ->
                        D.succeed Modified

                    "removed" ->
                        D.succeed Removed

                    _ ->
                        D.fail ("Invalid ChangeType" ++ str)
            )


type GrainChange
    = GrainChange ChangeType Grain


decoder : Decoder GrainChange
decoder =
    DecodeX.start GrainChange
        |> required "type" changeTypeDecoder
        |> required "doc" Grain.decoder


listDecoder : Decoder (List GrainChange)
listDecoder =
    D.list decoder


grain (GrainChange _ grain_) =
    grain_


type_ (GrainChange changeType _) =
    changeType
