module GrainStore exposing (GrainStore, updateF)

import Grain exposing (Grain)
import Random exposing (Generator, Seed)


type alias Model =
    { list : List Grain
    , seed : Seed
    }


type GrainStore
    = GrainStore Model


unwrap (GrainStore model) =
    model


map fn =
    unwrap >> fn >> GrainStore


type Msg
    = NoOp
    | CreateNew


initWithSeed seed =
    GrainStore { list = [], seed = seed }


generator : Generator GrainStore
generator =
    Random.independentSeed |> Random.map initWithSeed


updateF message =
    case message of
        NoOp ->
            identity

        CreateNew ->
            identity
