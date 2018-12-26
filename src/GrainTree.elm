module GrainTree exposing (Forest, GrainTree)

import Grain exposing (Grain)
import GrainCache exposing (GrainCache)
import GrainId exposing (GrainId)
import Tree


type alias Forest =
    List GrainTree


type alias GrainTree =
    Tree.Tree Grain



--type alias Path =
--    List GrainId
--
--
--forest : GrainCache -> Forest
--forest grainCache =
--    let
--        rootGrains =
--            GrainCache.rootGrains grainCache
--    in
--    rootGrains |> List.map (tree grainCache)
--
--
--tree : GrainCache -> Grain -> GrainTree
--tree grainCache grain =
--    let
--        newForest =
--            GrainCache.childGrains grain grainCache
--                |> List.map (tree grainCache)
--    in
--    Tree grain newForest
