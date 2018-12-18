module GrainStore exposing
    ( GrainStore
    , addNewGrain
    , allAsList
    , empty
    , getById
    , loadFromCache
    , onFirebaseChanges
    , permanentlyDeleteGrain
    , setGrainContent
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


getGrainHavingSameId =
    Grain.id >> getById


loadFromCache : Value -> GrainStore -> Return msg GrainStore
loadFromCache val gs =
    DecodeX.decodeWithDefault gs GrainDict.decoder val


hasGrainWithSameId grain =
    GrainDict.member (Grain.id grain)


setGrainContent content grain model =
    getGrainHavingSameId grain model
        |> Result.fromMaybe "Error: SetContent Grain Not Found in Cache"
        |> Result.map (Grain.setContent content >> updateGrain >> callWith model)


updateGrain grain =
    blindInsertGrain grain
        >> withUpdateGrainCmd grain


withUpdateGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistUpdatedGrain grain ] )


permanentlyDeleteGrain grain model =
    if hasGrainWithSameId grain model then
        blindRemoveGrain grain model
            |> withRemoveGrainCmd grain
            |> Result.Ok

    else
        Result.Err "Error: PermanentDeleteGrain: Grain Not Found in cache"


withRemoveGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistRemovedGrain grain ] )


addNewGrain : Grain -> GrainStore -> Result String ( GrainStore, Cmd msg )
addNewGrain grain model =
    let
        canAdd =
            hasGrainWithSameId grain model
                |> not
    in
    if canAdd then
        blindInsertGrain grain model
            |> withAddNewGrainCmd grain
            |> Result.Ok

    else
        Result.Err "Error: Add Grain. Id exists "


withAddNewGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistNewGrain grain ] )


cache =
    GrainDict.encoder >> Port.cacheGrains


blindInsertGrain grain =
    GrainDict.insert (Grain.id grain) grain


blindRemoveGrain grain =
    GrainDict.remove (Grain.id grain)


onFirebaseChanges changes model =
    let
        handleChange { doc, type_ } =
            case type_ of
                GrainChange.Added ->
                    blindInsertGrain doc

                GrainChange.Modified ->
                    blindInsertGrain doc

                GrainChange.Removed ->
                    blindRemoveGrain doc

        newModel =
            List.foldr handleChange model changes
    in
    ( newModel, cache newModel )
