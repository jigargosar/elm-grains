module GrainList exposing
    ( GrainList
    , cacheCmd
    , currentDomId
    , decodeString
    , filterMapCSToList
    , filteredRoll
    , filteredSelected
    , grainDomId
    , init
    , prepend
    , remove
    , update
    , toList
    )

import BasicsX exposing (..)
import Grain exposing (Grain)
import GrainFilter exposing (GrainFilter)
import GrainId exposing (GrainId)
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Iso as Iso exposing (Iso)
import Monocle.Lens as Lens exposing (Lens)
import Port
import SList exposing (SList)
import SelectList


type alias GSList =
    SList Grain


type alias Model =
    SList Grain


grainList2Model : Iso GrainList Model
grainList2Model =
    Iso (\(GrainList model) -> model) (\model -> GrainList model)


type GrainList
    = GrainList Model


init : GrainList
init =
    fromList []


fromList : List Grain -> GrainList
fromList =
    SList.fromList >> grainList2Model.reverseGet


decodeString : String -> Result D.Error GrainList
decodeString =
    D.decodeString (D.list Grain.decoder |> D.map fromList)


cacheCmd : GrainList -> Cmd msg
cacheCmd =
    grainList2Model.get >> SList.toList >> E.list Grain.encoder >> Port.cacheGrainList


over : (GSList -> GSList) -> GrainList -> GrainList
over =
    Iso.modify grainList2Model


remove : GrainId -> GrainList -> GrainList
remove =
    over << SList.reject << Grain.hasId


prepend : Grain -> GrainList -> GrainList
prepend =
    over << SList.prepend


update : GrainId -> (Grain -> Grain) -> GrainList -> GrainList
update gid fn =
    over <| SList.updateIf (Grain.hasId gid) fn


filterMapCSToList : (Grain -> Bool) -> (Grain -> a) -> (Grain -> a) -> GrainList -> List a
filterMapCSToList pred cFn sFn =
    unwrap >> SList.filterMapCSToList pred cFn sFn

toList =
    unwrap >> SList.toList

filteredRoll pred offset =
    over
        (SList.filterAndRollBy pred offset)


currentDomId : (Grain -> Bool) -> GrainList -> Maybe String
currentDomId pred =
    unwrap
        >> SList.filteredSelected pred
        >> Maybe.map grainDomId


unwrap : GrainList -> Model
unwrap (GrainList model) =
    model


filteredSelected pred =
    unwrap >> SList.filteredSelected pred


grainDomId =
    Grain.id >> GrainId.asString
