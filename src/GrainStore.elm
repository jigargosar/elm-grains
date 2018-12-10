module GrainStore exposing (GrainStore, Msg, allAsList, generator, updateF)

import Grain exposing (Grain)
import Random exposing (Generator, Seed)
import UpdateHandler exposing (HandlerConfig)


type alias Model =
    { list : List Grain
    , seed : Seed
    }


type GrainStore
    = GrainStore Model


initWithSeed seed =
    GrainStore { list = [], seed = seed }


generator : Generator GrainStore
generator =
    Random.independentSeed |> Random.map initWithSeed


unwrap (GrainStore model) =
    model


allAsList =
    unwrap >> .list


map fn =
    unwrap >> fn >> GrainStore


type Msg
    = NoOp
    | CreateNew


updateF : Msg -> HandlerConfig Msg GrainStore -> HandlerConfig Msg GrainStore
updateF message =
    case message of
        NoOp ->
            identity

        CreateNew ->
            identity
