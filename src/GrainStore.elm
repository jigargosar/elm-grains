module GrainStore exposing
    ( GrainStore
    , OutMsg(..)
    , UserMsg
    , addNewGrain
    , allAsList
    , empty
    , get
    , loadFromCache
    , onFirebaseChanges
    , permanentlyDeleteGrain
    , setGrainContent
    , setGrainDeleted
    , userUpdate
    )

import ActorId exposing (ActorId)
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
import RandomX
import Return exposing (Return)
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


grainList2GrainStore : List Grain -> GrainStore
grainList2GrainStore =
    List.map (\grain -> ( grainToGidString grain, grain )) >> Dict.fromList


loadFromCache : Value -> GrainStore -> Return msg GrainStore
loadFromCache val gs =
    DecodeX.decodeWithDefault gs (D.dict Grain.decoder) val


setGrainContent content =
    Update (SetContent content)


setGrainDeleted deleted =
    Update (SetDeleted deleted)


permanentlyDeleteGrain =
    DeletePermanent


addNewGrain =
    AddNew


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


type UserMsg
    = AddNew
    | Update UpdateGrain
    | DeletePermanent


type OutMsg
    = Error String
    | Added Grain
    | Modified Grain
    | PermanentlyDeleted Grain


withErrorOutMsg err r2 =
    ( r2, Error err )


userUpdate :
    UserMsg
    -> Grain
    -> ActorId
    -> GrainStore
    -> ( ( GrainStore, Cmd msg ), OutMsg )
userUpdate request grain actorId model =
    let
        gid =
            Grain.id grain

        gidAsString =
            GrainId.toString gid
    in
    case request of
        AddNew ->
            addNewGrainInternal grain model
                |> Maybe.unpack
                    (\_ ->
                        Return.singleton model
                            |> withErrorOutMsg "Error: Add Grain. Id exists "
                    )
                    (\( addedGrain, newModel ) ->
                        ( ( newModel
                          , Cmd.batch [ cache newModel, Firebase.persistNewGrain addedGrain ]
                          )
                        , Added addedGrain
                        )
                    )

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

        DeletePermanent ->
            removeExistingGrainById gid model
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
