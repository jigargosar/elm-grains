module GrainTree exposing
    ( GrainForest
    , GrainTree
    , GrainZipper
    , findFromRoot
    )

import Grain exposing (Grain)
import Tree exposing (Tree)
import Tree.Zipper as Z exposing (Zipper)


type alias GrainForest =
    List GrainTree


type alias GrainTree =
    Tree Grain


type alias GrainZipper =
    Zipper Grain


findFromRoot gid =
    Z.findFromRoot (Grain.idEq gid)
