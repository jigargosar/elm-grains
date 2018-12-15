module GrainStore exposing
    ( GrainStore
    , UpdateGrain(..)
    , UserChangeRequest(..)
    , allAsList
    , empty
    , get
    , loadFromCache
    , onFirebaseChanges
    , onUserChangeRequest
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


cache =
    E.dict identity Grain.encoder >> Port.cacheGrains


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

        gidAsString =
            GrainId.toString gid
    in
    case request of
        Add ->
            let
                newModel =
                    Dict.insert gidAsString grain model

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
                                    Dict.update gidAsString
                                        (Maybe.map <| Grain.setContent content)
                                        model
                            in
                            ( get gid newLookup |> Maybe.withDefault grain, newLookup )

                        SetDeleted deleted ->
                            let
                                newLookup =
                                    Dict.update gidAsString
                                        (Maybe.map <| Grain.setDeleted deleted)
                                        model
                            in
                            ( get gid newLookup |> Maybe.withDefault grain, newLookup )
            in
            ( newModel
            , Cmd.batch [ cache newModel, Firebase.persistUpdatedGrain updatedGrain ]
            )

        DeletePermanent ->
            let
                newModel =
                    Dict.remove gidAsString model
            in
            ( newModel, Cmd.batch [ cache newModel, Firebase.persistRemovedGrain grain ] )


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
