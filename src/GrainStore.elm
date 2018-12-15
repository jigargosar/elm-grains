module GrainStore exposing
    ( GrainStore
    , UpdateGrain(..)
    , UserChangeRequest(..)
    , addNewGrain
    , allAsList
    , empty
    , get
    , loadFromCache
    , onFirebaseChanges
    , onUserChangeRequest
    , permanentlyDeleteGrain
    , setGrainContent
    , setGrainDeleted
    )

import BasicsX exposing (callWith, unwrapMaybe)
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
import Return3 as R3 exposing (Return3F)


type alias GrainStore =
    Dict String Grain


empty =
    Dict.empty


allAsList =
    Dict.values


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    Dict.get (GrainId.toString gid)


loadFromCache val gs =
    DecodeX.decode gs (D.dict Grain.decoder) val


setGrainContent content =
    onUserChangeRequest (Update (SetContent content))


setGrainDeleted deleted =
    onUserChangeRequest (Update (SetDeleted deleted))


permanentlyDeleteGrain =
    onUserChangeRequest DeletePermanent


addNewGrain =
    onUserChangeRequest Add


cache =
    E.dict identity Grain.encoder >> Port.cacheGrains


updateExistingGrainById :
    GrainId
    -> (Grain -> Grain)
    -> GrainStore
    -> Maybe ( Grain, GrainStore )
updateExistingGrainById gid fn model =
    get gid model
        |> Maybe.map
            (fn
                >> (\updatedGrain ->
                        ( updatedGrain, blindUpsertGrain updatedGrain model )
                   )
            )


updateExistingGrain :
    Grain
    -> (Grain -> Grain)
    -> GrainStore
    -> Maybe ( Grain, GrainStore )
updateExistingGrain grain =
    updateExistingGrainById (Grain.id grain)


blindUpsertGrain grain model =
    Dict.insert (grainToGidString grain) grain model


blindRemoveGrain grain model =
    Dict.remove (grainToGidString grain) model


hasIdOfGrain grain =
    Dict.member (grainToGidString grain)


addNewGrainInternal : Grain -> GrainStore -> Maybe ( Grain, GrainStore )
addNewGrainInternal grain model =
    if hasIdOfGrain grain model then
        Nothing

    else
        Just <| ( grain, blindUpsertGrain grain model )


removeExistingGrainById :
    GrainId
    -> GrainStore
    -> Maybe ( Grain, GrainStore )
removeExistingGrainById gid model =
    get gid model
        |> Maybe.map
            (\removedGrain ->
                ( removedGrain, blindRemoveGrain removedGrain model )
            )


type UpdateGrain
    = SetDeleted Bool
    | SetContent String


type UserChangeRequest
    = Add
    | Update UpdateGrain
    | DeletePermanent


type UserChangeResponse
    = Added
    | Updated
    | Deleted


onUserChangeRequest :
    UserChangeRequest
    -> Grain
    -> GrainStore
    -> Result String ( GrainStore, Cmd msg )
onUserChangeRequest request grain model =
    let
        gid =
            Grain.id grain

        gidAsString =
            GrainId.toString gid
    in
    case request of
        Add ->
            addNewGrainInternal grain model
                |> Maybe.map
                    (\( addedGrain, newModel ) ->
                        ( newModel
                        , Cmd.batch [ cache newModel, Firebase.persistNewGrain addedGrain ]
                        )
                    )
                |> Result.fromMaybe "Error: Add Grain. Id exists "

        Update updateRequest ->
            let
                grainMapper =
                    case updateRequest of
                        SetContent content ->
                            Grain.setContent content

                        SetDeleted deleted ->
                            Grain.setDeleted deleted
            in
            updateExistingGrainById gid grainMapper model
                |> Maybe.map
                    (\( updatedGrain, newModel ) ->
                        ( newModel
                        , Cmd.batch
                            [ cache newModel, Firebase.persistUpdatedGrain updatedGrain ]
                        )
                    )
                |> Result.fromMaybe "Error: Update Grain. Not Found "

        DeletePermanent ->
            removeExistingGrainById gid model
                |> Maybe.map
                    (\( removedGrain, newModel ) ->
                        ( newModel
                        , Cmd.batch [ cache newModel, Firebase.persistRemovedGrain grain ]
                        )
                    )
                |> Result.fromMaybe "Error: DeletePermanent Grain. Not Found "


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
