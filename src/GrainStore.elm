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
import Dict exposing (Dict)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
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
    Dict String Grain


empty =
    Dict.empty


allAsList =
    Dict.values


getById : GrainId -> GrainStore -> Maybe Grain
getById gid =
    Dict.get (GrainId.toString gid)


getGrainHavingSameId =
    Grain.id >> getById


loadFromCache : Value -> GrainStore -> Return msg GrainStore
loadFromCache val gs =
    DecodeX.decodeWithDefault gs (D.dict Grain.decoder) val


hasGrainWithSameId grain =
    Dict.member (Grain.idString grain)


setGrainContent content grain model =
    getGrainHavingSameId grain model
        |> Result.fromMaybe "Error: SetContent Grain Not Found in Cache"
        |> Result.map (Grain.setContent content >> updateGrain >> callWith model)


updateGrain grain =
    blindUpsertGrain grain
        >> withUpdateGrainCmd grain


withUpdateGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistUpdatedGrain grain ] )


mapGrain fn grain model =
    if hasGrainWithSameId grain model then
        blindUpsertGrain grain model
            |> withRemoveGrainCmd grain
            |> Result.Ok

    else
        Result.Err "Error: PermanentDeleteGrain: Grain Not Found in cache"


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
        blindUpsertGrain grain model
            |> withAddNewGrainCmd grain
            |> Result.Ok

    else
        Result.Err "Error: Add Grain. Id exists "


withAddNewGrainCmd grain model =
    ( model, Cmd.batch [ cache model, Firebase.persistNewGrain grain ] )


cache =
    E.dict identity Grain.encoder >> Port.cacheGrains


updateExistingGrain :
    Grain
    -> (Grain -> Grain)
    -> GrainStore
    -> Maybe ( Grain, GrainStore )
updateExistingGrain grain fn model =
    getGrainHavingSameId grain model
        |> Maybe.map
            (fn
                >> (\updatedGrain ->
                        ( updatedGrain, blindUpsertGrain updatedGrain model )
                   )
            )


blindUpsertGrain grain =
    Dict.insert (Grain.idString grain) grain


blindRemoveGrain grain =
    Dict.remove (Grain.idString grain)


hasIdOfGrain grain =
    Dict.member (Grain.idString grain)


removeExistingGrainById :
    GrainId
    -> GrainStore
    -> Maybe ( Grain, GrainStore )
removeExistingGrainById gid model =
    getById gid model
        |> Maybe.map
            (\removedGrain ->
                ( removedGrain, blindRemoveGrain removedGrain model )
            )


onFirebaseChanges changes model =
    let
        handleChange { doc, type_ } =
            let
                gidAsString =
                    Grain.idString doc
            in
            case type_ of
                GrainChange.Added ->
                    Dict.insert gidAsString doc

                GrainChange.Modified ->
                    Dict.insert gidAsString doc

                GrainChange.Removed ->
                    Dict.remove gidAsString

        newModel =
            List.foldr handleChange model changes
    in
    ( newModel, cache newModel )
