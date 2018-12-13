module GrainLookup exposing
    ( GrainLookup
    , asList
    , decoder
    , encoder
    , get
    , init
    , remove
    , update
    , upsert
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Grain exposing (Grain)
import GrainId
import Json.Decode as D
import Json.Encode as E
import List.Extra as List


type alias Model =
    Dict String Grain


type GrainLookup
    = GrainLookup Model


unwrap (GrainLookup model) =
    model


map fn =
    unwrap >> fn >> GrainLookup


init =
    Dict.empty |> GrainLookup


asList =
    unwrap >> Dict.values


get gid =
    unwrap >> Dict.get (GrainId.toString gid)


upsert grain =
    map <| Dict.insert (Grain.id grain |> GrainId.toString) grain


encoder =
    unwrap >> E.dict identity Grain.encoder


decoder =
    D.dict Grain.decoder
        |> D.map GrainLookup


remove gid =
    map <| Dict.remove (GrainId.toString gid)


update gid fn =
    map <| Dict.update (GrainId.toString gid) (Maybe.map fn)
