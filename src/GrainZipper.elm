module GrainZipper exposing
    ( GrainForest
    , GrainTree
    , GrainZipper
    , flatten
    , fromTree
    , removeEqById
    )

import Grain exposing (Grain)
import Maybe.Extra as Maybe
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


mapMaybe fn =
    unwrap >> fn >> Maybe.map GrainZipper


fromTree : GrainTree -> GrainZipper
fromTree =
    Z.fromTree >> GrainZipper


findFromRootEqById : Grain -> GrainZipper -> Maybe GrainZipper
findFromRootEqById grain =
    Z.findFromRoot (Grain.eqById grain)
        |> mapMaybe


removeTree : GrainZipper -> Maybe GrainZipper
removeTree =
    Z.removeTree
        |> mapMaybe


removeEqById : Grain -> GrainZipper -> Maybe GrainZipper
removeEqById grain =
    findFromRootEqById grain
        >> Maybe.andThen removeTree


flatten : GrainZipper -> List Grain
flatten =
    unwrap >> Z.toTree >> Tree.flatten
