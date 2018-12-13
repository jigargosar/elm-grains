module GrainStore exposing
    ( GrainStore
    , Msg
    , Reply(..)
    , allAsList
    , createNewGrain
    , deleteGrain
    , firestoreChanges
    , generator
    , get
    , load
    , setContent
    , update
    )

import BasicsX exposing (callWith, unwrapMaybe)
import DecodeX exposing (Encoder)
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import Return3 as R3 exposing (Return3F)


type alias GrainStore =
    { list : List Grain
    , seed : Seed
    }


initWithSeed : Seed -> GrainStore
initWithSeed seed =
    { list = [], seed = seed }


generator : Generator GrainStore
generator =
    Random.independentSeed |> Random.map initWithSeed


allAsList =
    .list


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    .list >> List.find (Grain.idEq gid)


setSeed seed =
    \model -> { model | seed = seed }


mapList fn model =
    { model | list = fn model.list }


addGrain grain =
    mapList ((::) grain)


addGrainWithNewSeed grain seed =
    addGrain grain >> setSeed seed


createNewGrain =
    CreateNew


type GrainUpdateMsg
    = SetContent String
    | FromFireStoreChange GrainChange


type Msg
    = NoOp
    | CreateNew
    | Load Value
    | UpdateGrainId GrainId GrainUpdateMsg
    | DeleteGrainId GrainId
    | Firestore (List GrainChange)
    | UpdateAll (List ( GrainId, GrainUpdateMsg ))


updateGrain grain =
    UpdateGrainId (Grain.id grain)


setContent grain title =
    updateGrain grain <| SetContent title


deleteGrain grain =
    DeleteGrainId (Grain.id grain)


firestoreChanges =
    Firestore


load value =
    Load value


type Reply
    = NoReply
    | NewGrainAddedReply Grain


cache =
    R3.effect (encoder >> Port.cacheGrains)


persist =
    R3.effect (encoder >> Port.persistGrains)


cacheAndPersistR3 =
    cache >> persist


encoder : Encoder GrainStore
encoder model =
    E.object
        [ ( "list", E.list Grain.encoder model.list ) ]


decoder : Seed -> Decoder GrainStore
decoder seed =
    DecodeX.start GrainStore
        |> required "list" (D.list Grain.decoder)
        |> hardcoded seed


removeByGidR3 gid =
    R3.map (mapList (List.filterNot (Grain.idEq gid)))


update : Msg -> Return3F Msg GrainStore Reply
update message =
    case message of
        NoOp ->
            identity

        Load val ->
            R3.andThen
                (\model ->
                    let
                        r2 : ( GrainStore, Cmd Msg )
                        r2 =
                            DecodeX.decode model (decoder model.seed) val
                    in
                    R3.mergeR2 r2
                )

        CreateNew ->
            R3.andThen
                (\model ->
                    let
                        ( newGrain, newSeed ) =
                            Random.step Grain.generator model.seed
                    in
                    R3.map (addGrainWithNewSeed newGrain newSeed)
                        >> cacheAndPersistR3
                        >> R3.reply (NewGrainAddedReply newGrain)
                )

        UpdateGrainId gid msg ->
            update <| UpdateAll [ ( gid, msg ) ]

        DeleteGrainId gid ->
            removeByGidR3 gid
                >> cacheAndPersistR3

        Firestore changes ->
            List.map (\c -> ( Grain.id c.doc, FromFireStoreChange c )) changes
                |> UpdateAll
                |> update

        UpdateAll changes ->
            let
                updateOne ( gid, msg ) =
                    let
                        updateGrainR3 fn =
                            R3.map (mapList (List.updateIf (Grain.idEq gid) fn))
                    in
                    case msg of
                        SetContent title ->
                            updateGrainR3 (Grain.setContent title)

                        FromFireStoreChange { doc, type_ } ->
                            case type_ of
                                GrainChange.Added ->
                                    R3.andThen (upsert gid doc)

                                GrainChange.Modified ->
                                    R3.andThen (upsert gid doc)

                                GrainChange.Removed ->
                                    removeByGidR3 gid
            in
            List.foldr updateOne
                >> callWith changes
                >> cacheAndPersistR3


upsert gid grain model =
    let
        replaceByGid =
            List.setIf (Grain.idEq gid)

        mapper list_ =
            get gid model
                |> unwrapMaybe ((::) grain) replaceByGid
                |> callWith list_
    in
    R3.map (mapList mapper)
