module GrainStore exposing
    ( GrainStore
    , Msg
    , Reply(..)
    , allAsList
    , createNewGrain
    , generator
    , update
    )

import Grain exposing (Grain)
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


createNewGrain =
    CreateNew


type Msg
    = NoOp
    | CreateNew


type Reply
    = NoReply


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
                    R3.map (setSeed newSeed)
                )
