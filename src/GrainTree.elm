module GrainTree exposing (Forest, GrainTree(..))

import Grain exposing (Grain)
import GrainCache exposing (GrainCache)


type alias Forest =
    List GrainTree


type GrainTree
    = Tree Grain Forest


forestFromGrainCache : GrainCache -> Forest
forestFromGrainCache grainCache =
    let
        rootGrains =
            GrainCache.rootGrains grainCache
    in
    rootGrains |> List.map (treeFromGrainCache grainCache)


treeFromGrainCache : GrainCache -> Grain -> GrainTree
treeFromGrainCache grainCache grain =
    let
        children =
            GrainCache.childGrains grain
    in
    Tree grain (children |> List.map (treeFromGrainCache grainCache))
