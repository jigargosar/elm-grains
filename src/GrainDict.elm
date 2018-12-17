module GrainDict exposing
    ( GrainDict
    , decoder
    , empty
    , encoder
    , get
    , insert
    , remove
    , update
    , values
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


type GrainDict
    = GrainDict Model


unwrap (GrainDict model) =
    model


map fn =
    unwrap >> fn >> GrainDict


empty =
    Dict.empty |> GrainDict


values =
    unwrap >> Dict.values


get gid =
    unwrap >> Dict.get (GrainId.toString gid)


insert grain =
    map <| Dict.insert (Grain.id grain |> GrainId.toString) grain


encoder =
    unwrap >> E.dict identity Grain.encoder


decoder =
    D.dict Grain.decoder
        |> D.map GrainDict


remove gid =
    map <| Dict.remove (GrainId.toString gid)


update gid fn =
    map <| Dict.update (GrainId.toString gid) (Maybe.map fn)
