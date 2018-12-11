module GrainStore exposing
    ( GrainStore
    , Msg
    , Reply(..)
    , allAsList
    , createNewGrain
    , generator
    , get
    , load
    , setTitle
    , update
    )

import DecodeX exposing (Encoder)
import Grain exposing (Grain)
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
    = SetTitle String


type Msg
    = NoOp
    | CreateNew
    | Load Value
    | UpdateGrain GrainId GrainUpdateMsg


setTitle gid title =
    UpdateGrain gid <| SetTitle title


load =
    Load


type Reply
    = NoReply
    | NewGrainAddedReply Grain


cache : GrainStore -> Cmd msg
cache =
    encoder >> Port.cacheGrains


encoder : Encoder GrainStore
encoder model =
    E.object
        [ ( "list", E.list Grain.encoder model.list ) ]


decoder : Seed -> Decoder GrainStore
decoder seed =
    DecodeX.start GrainStore
        |> required "list" (D.list Grain.decoder)
        |> hardcoded seed


updateGrainWithId gid fn =
    mapList (List.updateIf (Grain.idEq gid) fn)


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
                        >> R3.effect cache
                        >> R3.reply (NewGrainAddedReply newGrain)
                )

        UpdateGrain gid msg ->
            case msg of
                SetTitle title ->
                    R3.map (updateGrainWithId gid (Grain.setTitle title))
                        >> R3.effect cache
