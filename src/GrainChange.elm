module GrainChange exposing (Change(..), GrainChange, changeDecoder, grainChangeDecoder, listDecoder)

import DecodeX
import Grain exposing (Grain)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E
import List.Extra as List
import Random exposing (Generator)
import RandomX


type Change
    = Added
    | Modified
    | Removed


changeDecoder =
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


type alias GrainChange =
    { type_ : Change, doc : Grain }


grainChangeDecoder : Decoder GrainChange
grainChangeDecoder =
    DecodeX.start GrainChange
        |> required "type" changeDecoder
        |> required "doc" Grain.decoder


listDecoder : Decoder (List GrainChange)
listDecoder =
    D.list grainChangeDecoder
