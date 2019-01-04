module GrainTree exposing
    ( GrainForest
    , GrainTree
    , GrainZipper
    , findFromRoot
    , treeAtGid
    )

import Grain exposing (Grain)
import GrainId exposing (GrainId)
import Tree exposing (Tree)
import Tree.Zipper as Z exposing (Zipper)


type alias GrainForest =
    List GrainTree


type alias GrainTree =
    Tree Grain


type alias GrainZipper =
    Zipper Grain


findFromRoot : GrainId -> GrainZipper -> Maybe GrainZipper
findFromRoot gid =
    Z.findFromRoot (Grain.idEq gid)


zipperFromTree =
    Z.fromTree


treeAtGid : GrainId -> GrainTree -> Maybe GrainTree
treeAtGid gid =
    zipperFromTree >> findFromRoot gid >> Maybe.map Z.tree
