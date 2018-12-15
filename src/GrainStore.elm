module GrainStore exposing
    ( GrainStore
    , UpdateGrain(..)
    , UserChangeRequest(..)
    , allAsList
    , get
    , init
    , loadFromCache
    , onFirebaseChanges
    , onUserChangeRequest
    )

import BasicsX exposing (callWith, unwrapMaybe)
import DecodeX exposing (Encoder)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainLookup exposing (GrainLookup)
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
    GrainLookup


init =
    GrainLookup.init


allAsList =
    GrainLookup.asList


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    GrainLookup.get gid


loadFromCache val gs =
    DecodeX.decode gs GrainLookup.decoder val


cache =
    GrainLookup.encoder >> Port.cacheGrains


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
    -> ( GrainStore, Cmd msg )
onUserChangeRequest request grain model =
    let
        gid =
            Grain.id grain
    in
    case request of
        Add ->
            let
                newModel =
                    GrainLookup.upsert grain model

                addedGrain =
                    grain
            in
            ( newModel
            , Cmd.batch [ cache newModel, Firebase.persistNewGrain addedGrain ]
            )

        Update updateRequest ->
            let
                ( updatedGrain, newModel ) =
                    case updateRequest of
                        SetContent content ->
                            let
                                newLookup =
                                    GrainLookup.update gid (Grain.setContent content) model
                            in
                            ( get gid newLookup |> Maybe.withDefault grain, newLookup )

                        SetDeleted deleted ->
                            let
                                newLookup =
                                    GrainLookup.update gid (Grain.setDeleted deleted) model
                            in
                            ( get gid newLookup |> Maybe.withDefault grain, newLookup )
            in
            ( newModel
            , Cmd.batch [ cache newModel, Firebase.persistUpdatedGrain updatedGrain ]
            )

        DeletePermanent ->
            let
                newModel =
                    GrainLookup.remove gid model
            in
            ( newModel, Cmd.batch [ cache newModel, Firebase.persistRemovedGrain grain ] )


onFirebaseChanges changes model =
    let
        handleChange { doc, type_ } =
            case type_ of
                GrainChange.Added ->
                    GrainLookup.upsert doc

                GrainChange.Modified ->
                    GrainLookup.upsert doc

                GrainChange.Removed ->
                    GrainLookup.remove (Grain.id doc)

        newModel =
            List.foldr handleChange model changes
    in
    ( newModel, cache newModel )
