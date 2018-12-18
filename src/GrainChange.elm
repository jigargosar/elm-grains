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


type alias Model =
    { type_ : ChangeType
    , grain : Grain
    }


type GrainChange
    = GrainChange Model


unwrap (GrainChange model) =
    model


map fn =
    unwrap >> fn >> GrainChange


decoder : Decoder GrainChange
decoder =
    DecodeX.start Model
        |> required "type" changeTypeDecoder
        |> required "doc" Grain.decoder
        |> D.map GrainChange


listDecoder : Decoder (List GrainChange)
listDecoder =
    D.list decoder


grain =
    unwrap >> .grain


type_ =
    unwrap >> .type_
