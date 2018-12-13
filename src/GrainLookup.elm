module GrainLookup exposing (GrainLookup, asList, decoder, encoder, get, init, insert, remove, replace, update)

import Dict exposing (Dict)
import Grain exposing (Grain)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List



--type alias Model =
--    { dict : Dict String Grain
--    }
--type GrainLookup
--    = GrainLookup Model
--unwrap (GrainLookup model) =
--    model
--
--
--map fn =
--    unwrap >> fn >> GrainLookup
--
--
--init =
--    Dict.empty


type alias GrainLookup =
    List Grain


init =
    []


asList =
    identity


get gid =
    List.find (Grain.idEq gid)


insert grain =
    (::) grain


encoder =
    E.list Grain.encoder


decoder =
    D.list Grain.decoder


remove gid =
    List.filterNot (Grain.idEq gid)


update gid fn =
    List.updateIf (Grain.idEq gid) fn


replace gid grain =
    List.setIf (Grain.idEq gid) grain
