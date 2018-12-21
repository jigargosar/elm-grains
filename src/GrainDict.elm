module GrainDict exposing
    ( GrainDict
    , decoder
    , empty
    , encoder
    , get
    , insert
    , member
    , remove
    , update
    , values
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Grain exposing (Grain)
import GrainId
import GrainIdLookup exposing (GrainIdLookup)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List


type alias Model =
    GrainIdLookup Grain


type GrainDict
    = GrainDict Model


unwrap (GrainDict model) =
    model


map fn =
    unwrap >> fn >> GrainDict


empty =
    GrainIdLookup.empty |> GrainDict


values =
    unwrap >> GrainIdLookup.toList


get gid =
    unwrap >> GrainIdLookup.get gid


insert gid grain =
    map <| GrainIdLookup.insert gid grain


encoder =
    unwrap >> GrainIdLookup.toList >> E.list Grain.encoder


decoder =
    D.list Grain.decoder
        |> D.map (GrainIdLookup.fromList Grain.id)
        |> D.map GrainDict


remove gid =
    map <| GrainIdLookup.remove gid


update gid fn =
    map <| GrainIdLookup.updateIfExists gid fn


member gid =
    unwrap >> GrainIdLookup.member gid
