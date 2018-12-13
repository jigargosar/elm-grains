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
    { lookup : GrainLookup
    , seed : Seed
    }


initWithSeed : Seed -> GrainStore
initWithSeed seed =
    { lookup = GrainLookup.init, seed = seed }


generator : Generator GrainStore
generator =
    Random.independentSeed |> Random.map initWithSeed


allAsList =
    .lookup >> GrainLookup.asList


lookup =
    .lookup


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    lookup >> GrainLookup.get gid


setSeed seed =
    \model -> { model | seed = seed }


mapLookup : (GrainLookup -> GrainLookup) -> GrainStore -> GrainStore
mapLookup fn model =
    { model | lookup = fn model.lookup }


addGrain grain =
    mapLookup (GrainLookup.insert grain)


addGrainWithNewSeed grain seed =
    addGrain grain >> setSeed seed


createNewGrain =
    CreateNew


type GrainUpdateMsg
    = SetContent String


type Msg
    = NoOp
    | CreateNew
    | Load Value
    | UpdateGrainId GrainId GrainUpdateMsg
    | DeleteGrainId GrainId
    | Firestore (List GrainChange)


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
        [ ( "lookup", GrainLookup.encoder model.lookup ) ]


decoder : Seed -> Decoder GrainStore
decoder seed =
    DecodeX.start GrainStore
        |> required "lookup" GrainLookup.decoder
        |> hardcoded seed


removeByGidR3 gid =
    R3.map (mapLookup <| GrainLookup.remove gid)


type alias ReturnF =
    Return3F Msg GrainStore Reply


update : Msg -> ReturnF
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
            let
                updateGrainR3 fn =
                    R3.map (mapLookup (GrainLookup.update gid fn))
            in
            case msg of
                SetContent title ->
                    updateGrainR3 (Grain.setContent title)
                        >> cacheAndPersistR3

        DeleteGrainId gid ->
            removeByGidR3 gid
                >> cacheAndPersistR3

        Firestore changes ->
            let
                updateOne { doc, type_ } =
                    case type_ of
                        GrainChange.Added ->
                            R3.andThen (upsert doc)

                        GrainChange.Modified ->
                            R3.andThen (upsert doc)

                        GrainChange.Removed ->
                            removeByGidR3 (Grain.id doc)
            in
            List.foldr updateOne
                >> callWith changes
                >> cache


upsert : Grain -> GrainStore -> ReturnF
upsert grain model =
    let
        mapper : GrainLookup -> GrainLookup
        mapper =
            GrainLookup.insert grain
    in
    R3.map (mapLookup mapper)
