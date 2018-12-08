module GrainStore exposing (GrainStore, decodeString)

import DecodeX exposing (Encoder)
import Dict exposing (Dict)
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


type alias Model =
    { items : List Grain
    }


type GrainStore
    = GrainStore Model


unwrap (GrainStore model) =
    model


map fn =
    unwrap >> fn >> GrainStore


encoder : Encoder GrainStore
encoder (GrainStore model) =
    E.object
        [ ( "items", E.list Grain.encoder model.items )
        ]


decoder : Decoder GrainStore
decoder =
    DecodeX.start Model
        |> required "items" (D.list Grain.decoder)
        |> D.map GrainStore


decodeString str =
    D.decodeString decoder str
