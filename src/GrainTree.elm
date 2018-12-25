module GrainTree exposing (Forest, GrainTree(..))

import SavedGrain exposing (SavedGrain)


type alias Forest =
    List SavedGrain


type GrainTree
    = Tree SavedGrain Forest
