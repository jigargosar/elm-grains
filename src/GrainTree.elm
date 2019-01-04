module GrainTree exposing (GrainForest, GrainTree, GrainZipper)

import Grain exposing (Grain)
import Tree exposing (Tree)
import Tree.Zipper exposing (Zipper)


type alias GrainForest =
    List GrainTree


type alias GrainTree =
    Tree Grain


type alias GrainZipper =
    Zipper Grain
