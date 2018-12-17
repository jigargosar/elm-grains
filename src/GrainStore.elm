module GrainStore exposing
    ( GrainStore
    , OutMsg(..)
    , UpdateGrain
    , addNewGrain
    , allAsList
    , empty
    , getById
    , loadFromCache
    , onFirebaseChanges
    , permanentlyDeleteGrain
    , setGrainContent
    , userUpdate
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


get =
    Grain.id >> getById


grainList2GrainStore : List Grain -> GrainStore
grainList2GrainStore =
    List.map (\grain -> ( grainToGidString grain, grain )) >> Dict.fromList


loadFromCache : Value -> GrainStore -> Return msg GrainStore
loadFromCache val gs =
    DecodeX.decodeWithDefault gs (D.dict Grain.decoder) val


setGrainContent content =
    SetContent content


permanentlyDeleteGrain grain model =
    removeExistingGrainById (Grain.id grain) model
        |> Maybe.unpack
            (\_ ->
                Return.singleton model
                    |> withErrorOutMsg "Error: DeletePermanent Grain. Not Found "
            )
            (\( removedGrain, newModel ) ->
                ( ( newModel
                  , Cmd.batch [ cache newModel, Firebase.persistRemovedGrain grain ]
                  )
                , PermanentlyDeleted removedGrain
                )
            )


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


hasGrainWithSameId grain =
    Dict.member (Grain.idString grain)


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
    get grain model
        |> Maybe.map
            (fn
                >> (\updatedGrain ->
                        ( updatedGrain, blindUpsertGrain updatedGrain model )
                   )
            )


blindUpsertGrain grain model =
    Dict.insert (grainToGidString grain) grain model


blindRemoveGrain grain model =
    Dict.remove (grainToGidString grain) model


hasIdOfGrain grain =
    Dict.member (grainToGidString grain)


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


type UpdateGrain
    = SetContent String


type OutMsg
    = Error String
    | Added Grain
    | Modified Grain
    | PermanentlyDeleted Grain


withErrorOutMsg err r2 =
    ( r2, Error err )


userUpdate :
    UpdateGrain
    -> Grain
    -> GrainStore
    -> ( ( GrainStore, Cmd msg ), OutMsg )
userUpdate request grain model =
    let
        grainMapper =
            case request of
                SetContent content ->
                    Grain.setContent content
    in
    updateExistingGrain grain grainMapper model
        |> Maybe.unpack
            (\_ ->
                Return.singleton model
                    |> withErrorOutMsg "Error: Update Grain. Not Found "
            )
            (\( updatedGrain, newModel ) ->
                ( ( newModel
                  , Cmd.batch
                        [ cache newModel, Firebase.persistUpdatedGrain updatedGrain ]
                  )
                , Modified updatedGrain
                )
            )


grainToGidString =
    Grain.id >> GrainId.toString


onFirebaseChanges changes model =
    let
        handleChange { doc, type_ } =
            let
                gidAsString =
                    grainToGidString doc
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
