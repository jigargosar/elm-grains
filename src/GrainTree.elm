module GrainTree exposing (Forest, GrainTree, forest, tree)

import Grain exposing (Grain)
import GrainCache exposing (GrainCache)


type alias Forest =
    List GrainTree


type GrainTree
    = Tree Grain Forest


forest : GrainCache -> Forest
forest grainCache =
    let
        rootGrains =
            GrainCache.rootGrains grainCache
    in
    rootGrains |> List.map (tree grainCache)


tree : GrainCache -> Grain -> GrainTree
tree grainCache grain =
    let
        newForest =
            GrainCache.childGrains grain grainCache
                |> List.map (tree grainCache)
    in
    Tree grain newForest
