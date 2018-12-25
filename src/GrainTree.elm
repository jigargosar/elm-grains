module GrainTree exposing (Forest, GrainTree(..))

import Grain
import GrainCache exposing (GrainCache)
import SavedGrain exposing (SavedGrain)


type alias Forest =
    List GrainTree


type GrainTree
    = Tree SavedGrain Forest


forestFromGrainCache : GrainCache -> Forest
forestFromGrainCache grainCache =
    let
        allGrains =
            grainCache
                |> GrainCache.toList

        rootGrains =
            allGrains
                |> List.filter (SavedGrain.parentIdEq Grain.rootParentId)
                |> SavedGrain.defaultSort
    in
    rootGrains |> List.map (treeFromGrainCache grainCache)


treeFromGrainCache : GrainCache -> SavedGrain -> GrainTree
treeFromGrainCache grainCache savedGrain =
    let
        allSaved =
            grainCache |> GrainCache.toList

        directChildren =
            allSaved |> List.filter (SavedGrain.isChildOf savedGrain)
    in
    Tree savedGrain (directChildren |> List.map (treeFromGrainCache grainCache))
