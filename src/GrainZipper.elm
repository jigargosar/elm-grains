module GrainZipper exposing
    ( GrainForest
    , GrainTree
    , GrainZipper
    , fromTree
    )

import Grain exposing (Grain)
import Tree exposing (Tree)
import Tree.Zipper as Z exposing (Zipper)


type alias Model =
    Zipper Grain


type alias GrainForest =
    List GrainTree


type alias GrainTree =
    Tree Grain


type GrainZipper
    = GrainZipper Model


unwrap (GrainZipper model) =
    model


map fn =
    unwrap >> fn >> GrainZipper


fromTree : GrainTree -> GrainZipper
fromTree =
    Z.fromTree >> GrainZipper
