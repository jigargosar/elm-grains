module GrainStore exposing
    ( GrainStore
    , addNewGrain
    , allAsList
    , empty
    , getById
    , loadCache
    , onFirebaseChanges
    , permanentlyDeleteGrain
    , setContent
    , setDeleted
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
--  SET CONTENT


setContent now content grain model =
    getGrainHavingSameId grain model
        |> Result.fromMaybe "Error: SetContent Grain Not Found in Cache"
        |> Result.map
            (Grain.setContent content
                >> updateGrain now model
            )



-- SET DELETED


setDeleted now deleted grain model =
    getGrainHavingSameId grain model
        |> Result.fromMaybe "Error: setDeleted: Grain Not Found in Cache"
        |> Result.map
            (Grain.setDeleted deleted
                >> updateGrain now model
            )



-- UPDATE EXISTING HELPERS


updateGrain now model =
    Grain.setModifiedAt now
        >> insertGrainAndReturnWithUpdateGrainCmd
        >> callWith model


insertGrainAndReturnWithUpdateGrainCmd grain =
    blindInsert grain >> withUpdateGrainCmd grain


withUpdateGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistUpdatedGrain grain ] )



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
