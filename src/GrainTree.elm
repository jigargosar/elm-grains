module GrainTree exposing (Forest, GrainTree(..))

import Grain
import GrainCache
import SavedGrain exposing (SavedGrain)


type alias Forest =
    List SavedGrain


type GrainTree
    = Tree SavedGrain Forest


fromGrainCache grainCache =
    let
        sort =
            List.sortWith Grain.defaultComparator

        allGrains =
            grainCache
                |> GrainCache.toList
                |> List.map SavedGrain.value
                |> sort

        rootGrains =
            allGrains |> List.filter (Grain.parentIdEq Grain.rootParentId)
    in
    rootGrains
