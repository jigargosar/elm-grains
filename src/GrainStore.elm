module GrainStore exposing
    ( GrainStore
    , Msg
    , Reply(..)
    , allAsList
    , createNewGrain
    , generator
    , update
    )

import DecodeX exposing (Encoder)
import Grain exposing (Grain)
import Json.Decode as D
import Json.Encode as E
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


setSeed seed =
    \model -> { model | seed = seed }


addGrain grain =
    \model -> { model | list = grain :: model.list }


addGrainWithNewSeed grain seed =
    addGrain grain >> setSeed seed


createNewGrain =
    CreateNew


type Msg
    = NoOp
    | CreateNew


type Reply
    = NoReply
    | NewGrainAddedReply Grain


cache =
    encoder >> Port.cacheGrains


encoder : Encoder GrainStore
encoder model =
    E.object
        [ ( "list", E.list Grain.encoder model.list ) ]



--decoder : Decoder GrainStore
--decoder =
--    DecodeX.start GrainStore
--      |>


update : Msg -> Return3F Msg GrainStore Reply
update message =
    case message of
        NoOp ->
            identity

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
