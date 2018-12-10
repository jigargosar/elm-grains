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
import Return3 exposing (Return3F)


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


newMsg =
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
            identity
