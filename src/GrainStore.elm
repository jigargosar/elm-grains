module GrainStore exposing
    ( GrainStore
    , cacheCmd
    , decode
    , decoder
    , grainDomId
    , insertAt
    , items
    , prepend
    , update
    )

import BasicsX exposing (callWith, unwrapMaybe)
import DecodeX exposing (Encoder)
import Dict exposing (Dict)
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import List.Extra as List
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


decode =
    DecodeX.decode empty decoder


empty =
    GrainStore { items = [] }


unwrap (GrainStore model) =
    model


items =
    unwrap >> .items


cacheCmd : GrainStore -> Cmd msg
cacheCmd =
    encoder >> Port.cacheGrains


map fn =
    unwrap >> fn >> GrainStore


mapItems fn =
    map (\model -> { model | items = fn model.items })


prepend grain =
    mapItems ((::) grain)


insertAt ( insertPos, grain ) =
    mapItems
        (\list ->
            case insertPos of
                Grain.Head ->
                    list

                Grain.After gid ->
                    List.findIndex (Grain.hasId gid) list
                        |> unwrapMaybe list
                            ((+) 1
                                >> (\n ->
                                        List.take n list
                                            ++ [ grain ]
                                            ++ List.drop n list
                                   )
                            )
        )


grainDomId =
    Grain.id >> GrainId.asString


update gid fn =
    mapItems (List.updateIf (Grain.hasId gid) fn)
