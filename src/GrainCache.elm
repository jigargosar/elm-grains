module GrainCache exposing
    ( GrainCache
    , addNewGrain
    , batchUpdate
    , decoder
    , empty
    , encoder
    , get
    , isDescendent
    , moveBy
    , remove
    , setSaved
    , toList
    , updateWithGrainMsg
    )

import ActorId exposing (ActorId)
import BasicsX exposing (callWith, callWith2, ifElse, unwrapMaybe)
import Compare
import DecodeX exposing (Encoder)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainDict exposing (GrainDict)
import GrainId exposing (GrainId)
import GrainIdLookup exposing (GrainIdLookup)
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
import SavedGrain exposing (SavedGrain)
import Time exposing (Posix)


type alias GrainCache =
    GrainIdLookup SavedGrain


decoder : Decoder GrainCache
decoder =
    D.list SavedGrain.decoder
        |> D.map (GrainIdLookup.fromList SavedGrain.id)


encoder : GrainCache -> Value
encoder =
    GrainIdLookup.toList >> E.list SavedGrain.encoder


empty : GrainCache
empty =
    GrainIdLookup.empty


get : GrainId -> GrainCache -> Maybe SavedGrain
get gid =
    GrainIdLookup.get gid


getParentOfGrain : Grain -> GrainCache -> Maybe SavedGrain
getParentOfGrain grain model =
    Grain.parentIdAsGrainId grain
        |> Maybe.andThen (GrainIdLookup.get >> callWith model)


isDescendent : Grain -> Grain -> GrainCache -> Bool
isDescendent descendent ancestor model =
    if Grain.isParentOf descendent ancestor then
        True

    else
        getParentOfGrain descendent model
            |> Maybe.unwrap False
                (SavedGrain.value
                    >> isDescendent
                    >> callWith2 ancestor model
                )


getAncestorIdsOfGrain grain model =
    getAncestorIdsHelp [] grain model


getAncestorIdsHelp ids grain model =
    let
        gid =
            Grain.id grain

        newIds =
            gid :: ids
    in
    Grain.parentIdAsGrainId grain
        |> Maybe.andThen (get >> callWith model >> Maybe.map SavedGrain.value)
        |> Maybe.unwrap newIds (getAncestorIdsHelp newIds >> callWith model)


toList =
    GrainIdLookup.toList


setSaved : Grain -> GrainCache -> GrainCache
setSaved grain =
    GrainIdLookup.update (Grain.id grain)
        (Maybe.map (SavedGrain.setSaved grain)
            >> Maybe.orElseLazy (\_ -> Just <| SavedGrain.new grain)
        )


remove grain =
    GrainIdLookup.remove (Grain.id grain)


type alias UpdateResult =
    Result String GrainCache


addNewGrain grain model =
    let
        gid =
            Grain.id grain

        alreadyExists =
            GrainIdLookup.member gid model
    in
    if alreadyExists then
        Result.Err "Error: Add Grain. GrainId exists"

    else
        GrainIdLookup.insert gid (SavedGrain.new grain) model
            |> Result.Ok


update :
    (Grain -> Grain)
    -> GrainId
    -> GrainCache
    -> UpdateResult
update changeFn gid model =
    if GrainIdLookup.member gid model then
        let
            updateFn =
                SavedGrain.change changeFn
        in
        Result.Ok <| GrainIdLookup.updateIfExists gid updateFn model

    else
        Result.Err "GrainNotFound"


batchUpdate : List ( Grain -> Grain, GrainId ) -> GrainCache -> UpdateResult
batchUpdate list model =
    let
        reducer ( changeFn, gid ) =
            Result.andThen (update changeFn gid)
    in
    List.foldl reducer (Result.Ok model) list


updateWithGrainMsg :
    Grain.Update
    -> Posix
    -> GrainId
    -> GrainCache
    -> UpdateResult
updateWithGrainMsg grainUpdate now gid model =
    update (Grain.update now grainUpdate) gid model


moveBy : Int -> Posix -> GrainId -> GrainCache -> UpdateResult
moveBy offset now gid model =
    get gid model
        |> Result.fromMaybe "Error: setSortIdx: Grain Not Found in Cache"
        |> Result.andThen (moveHelp now offset >> callWith model)


moveHelp : Posix -> Int -> SavedGrain -> GrainCache -> UpdateResult
moveHelp now offset savedGrain model =
    let
        siblings : List SavedGrain
        siblings =
            getSiblings savedGrain model

        gIdx : Int
        gIdx =
            List.findIndex (eqById savedGrain)
                siblings
                |> Maybe.withDefault -1

        updaters : List ( Grain -> Grain, GrainId )
        updaters =
            List.swapAt gIdx (gIdx + offset) siblings
                |> List.map SavedGrain.value
                |> Grain.listToEffectiveSortIndices
                |> List.map
                    (Tuple.mapBoth
                        (Grain.SetSortIdx >> Grain.update now)
                        Grain.id
                    )
    in
    batchUpdate updaters model


getSiblings : SavedGrain -> GrainCache -> List SavedGrain
getSiblings savedGrain model =
    toList model
        |> List.filter (eqByParentId savedGrain)
        |> List.sortWith defaultComparator


defaultComparator =
    Compare.compose SavedGrain.value Grain.defaultComparator


eqByParentId savedGrain =
    SavedGrain.value
        >> Grain.eqByParentId (SavedGrain.value savedGrain)


eqById savedGrain =
    SavedGrain.value
        >> Grain.eqById (SavedGrain.value savedGrain)
