module GrainStore exposing
    ( GrainStore
    , addNewGrain
    , allAsList
    , empty
    , getAncestorIds
    , getById
    , loadCache
    , moveBy
    , onFirebaseChanges
    , permanentlyDeleteGrain
    , setContent
    , setDeleted
    , setParentId
    )

import ActorId exposing (ActorId)
import BasicsX exposing (callWith, ifElse, unwrapMaybe)
import DecodeX exposing (Encoder)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainDict exposing (GrainDict)
import GrainId exposing (GrainId)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import RandomX
import Return exposing (Return)
import Return3 as R3 exposing (Return3F)


type alias GrainStore =
    GrainDict


empty =
    GrainDict.empty


allAsList =
    GrainDict.values


getById : GrainId -> GrainStore -> Maybe Grain
getById gid =
    GrainDict.get gid


getAncestorIds grain model =
    getAncestorIdsHelp [] grain model


getAncestorIdsHelp ids grain model =
    let
        gid =
            Grain.id grain

        newIds =
            gid :: ids
    in
    Grain.parentIdAsGrainId grain
        |> Maybe.andThen (getById >> callWith model)
        |> Maybe.unwrap newIds (getAncestorIdsHelp newIds >> callWith model)



-- INTERNAL HELPERS


getGrainHavingSameId =
    Grain.id >> getById


hasGrainWithSameId grain =
    GrainDict.member (Grain.id grain)


blindInsert grain =
    GrainDict.insert (Grain.id grain) grain


blindRemove grain =
    GrainDict.remove (Grain.id grain)


encoder =
    GrainDict.encoder


decoder =
    GrainDict.decoder



--- CACHE


loadCache : Value -> GrainStore -> Return msg GrainStore
loadCache val gs =
    DecodeX.decodeWithDefault gs decoder val


cache =
    encoder >> Port.cacheGrains



-- INSERT NEW


addNewGrain : Grain -> GrainStore -> Result String ( GrainStore, Cmd msg )
addNewGrain grain model =
    let
        canAdd =
            hasGrainWithSameId grain model
                |> not
    in
    if canAdd then
        blindInsert grain model
            |> withAddNewGrainCmd grain
            |> Result.Ok

    else
        Result.Err "Error: Add Grain. Id exists "


withAddNewGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistNewGrain grain ] )



-- UPDATE EXISTING


updateGrain now msg model grain =
    let
        updatedGrain =
            Grain.update now msg grain

        newModel =
            blindInsert updatedGrain model
    in
    withUpdateGrainCmd updatedGrain newModel


withUpdateGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistUpdatedGrain grain ] )



--  SET CONTENT


setContent content now gid model =
    getById gid model
        |> Maybe.map (updateGrain now (Grain.SetContent content) model)
        |> Result.fromMaybe "Error: SetContent Grain Not Found in Cache"



-- SET DELETED


setDeleted deleted now gid model =
    getById gid model
        |> Maybe.map (updateGrain now (Grain.SetDeleted deleted) model)
        |> Result.fromMaybe "Error: setDeleted: Grain Not Found in Cache"



-- SET ParentId


setParentId parentId now gid model =
    getById gid model
        |> Maybe.map (updateGrain now (Grain.SetParentId parentId) model)
        |> Result.fromMaybe "Error: setParentId: Grain Not Found in Cache"



-- SET SortIdx


setSortIdx now sortIdx gid model =
    getById gid model
        |> Maybe.map (updateGrain now (Grain.SetSortIdx sortIdx) model)
        |> Result.fromMaybe "Error: setSortIdx: Grain Not Found in Cache"


moveBy now offset gid model =
    getById gid model
        |> Maybe.map (moveHelp now offset >> callWith model)
        |> Result.fromMaybe "Error: setSortIdx: Grain Not Found in Cache"


moveHelp now offset grain model =
    let
        siblings : List Grain
        siblings =
            getSiblings grain model

        gIdx : Int
        gIdx =
            List.findIndex (Grain.eqById grain) siblings
                |> Maybe.withDefault -1

        updatedGrains : List Grain
        updatedGrains =
            List.swapAt gIdx (gIdx + offset) siblings
                |> Grain.updateSortIndices now

        newModel =
            List.foldl blindInsert model updatedGrains

        fireCmd : Cmd msg
        fireCmd =
            updatedGrains
                |> List.map Firebase.persistUpdatedGrain
                |> Cmd.batch
    in
    ( newModel, Cmd.batch [ cache newModel, fireCmd ] )


getSiblings : Grain -> GrainStore -> List Grain
getSiblings grain model =
    allAsList model
        |> List.filter (Grain.eqByParentId grain)
        |> List.sortWith Grain.defaultComparator



-- PERMANENT DELETE EXISTING


permanentlyDeleteGrain grain model =
    if hasGrainWithSameId grain model then
        blindRemove grain model
            |> withRemoveGrainCmd grain
            |> Result.Ok

    else
        Result.Err "Error: PermanentDeleteGrain: Grain Not Found in cache"


withRemoveGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistRemovedGrain grain ] )



-- HANDLE FIREBASE GRAIN CHANGES


onFirebaseChanges changeList model =
    let
        handleChange change =
            let
                grain =
                    GrainChange.grain change
            in
            case GrainChange.type_ change of
                GrainChange.Added ->
                    blindInsert grain

                GrainChange.Modified ->
                    blindInsert grain

                GrainChange.Removed ->
                    blindRemove grain

        newModel =
            List.foldr handleChange model changeList
    in
    ( newModel, cache newModel )
