module GrainIdLookup exposing
    ( GrainIdLookup
    , empty
    , fromList
    , get
    , insert
    , member
    , remove
    , toList
    , updateIfExists
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List


type alias Model a =
    Dict String a


type GrainIdLookup a
    = GrainIdLookup (Model a)


unwrap (GrainIdLookup model) =
    model


map fn =
    unwrap >> fn >> GrainIdLookup


empty =
    Dict.empty |> GrainIdLookup


fromList : (a -> GrainId) -> List a -> GrainIdLookup a
fromList gidGetter =
    List.map insert >> GrainIdLookup


toList =
    unwrap >> Dict.values


get gid =
    unwrap >> Dict.get (GrainId.toString gid)


insert gid val =
    map <| Dict.insert (GrainId.toString gid) val


remove gid =
    map <| Dict.remove (GrainId.toString gid)


updateIfExists gid fn =
    map <| Dict.update (GrainId.toString gid) (Maybe.map fn)


member gid =
    unwrap >> Dict.member (GrainId.toString gid)
