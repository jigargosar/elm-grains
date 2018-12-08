module GrainStore exposing
    ( GrainStore
    , cacheCmd
    , decodeString
    , decoder
    , grainDomId
    , items
    , prepend
    )

import DecodeX exposing (Encoder)
import Dict exposing (Dict)
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Port


type alias Model =
    { items : List Grain
    }


type GrainStore
    = GrainStore Model


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


decodeString =
    DecodeX.decodeString empty decoder


empty =
    GrainStore { items = [] }


unwrap (GrainStore model) =
    model


items =
    unwrap >> .items


cacheCmd : GrainStore -> Cmd msg
cacheCmd =
    items >> E.list Grain.encoder >> Port.cacheGrains


map fn =
    unwrap >> fn >> GrainStore


mapItems fn =
    map (\model -> { model | items = fn model.items })


prepend grain =
    mapItems ((::) grain)


grainDomId =
    Grain.id >> GrainId.asString
