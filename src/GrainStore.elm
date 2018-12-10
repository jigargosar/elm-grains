module GrainStore exposing
    ( GrainStore
    , Msg
    , Reply(..)
    , allAsList
    , generator
    , newMsg
    , update
    )

import Grain exposing (Grain)
import Random exposing (Generator, Seed)
import Return3 as R3 exposing (Return3F)


type alias Model =
    { list : List Grain
    , seed : Seed
    }


type GrainStore
    = GrainStore Model


unwrap : GrainStore -> Model
unwrap (GrainStore model) =
    model


map fn =
    unwrap >> fn >> GrainStore


initWithSeed : Seed -> Model
initWithSeed seed =
    { list = [], seed = seed }


generator : Generator GrainStore
generator =
    Random.independentSeed
        |> Random.map (initWithSeed >> GrainStore)


allAsList =
    unwrap >> .list


setSeed seed =
    \model -> { model | seed = seed }


newMsg =
    CreateNew


type Msg
    = NoOp
    | CreateNew


type Reply
    = NoReply


update : Msg -> Return3F Msg GrainStore Reply
update message r3 =
    R3.map unwrap r3
        |> updateInternal message
        |> R3.map GrainStore


updateInternal : Msg -> Return3F Msg Model Reply
updateInternal message =
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
