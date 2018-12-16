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


grainChangeDecoder : Decoder (Generator GrainChange)
grainChangeDecoder =
    DecodeX.start (\type_ -> Random.map (GrainChange type_))
        |> required "type" changeDecoder
        |> required "doc" Grain.decoderGenerator


listDecoder : Decoder (Generator (List GrainChange))
listDecoder =
    D.list grainChangeDecoder
        |> D.map RandomX.listOfGeneratorToGeneratorOfList
