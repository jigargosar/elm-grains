module GrainZipper exposing
    ( GrainForest
    , GrainTree
    , GrainZipper
    , fromTree
    , lastDescendentGrain
    , removeEqByIdThenFlatten
    )

import Grain exposing (Grain)
import GrainId exposing (GrainId)
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


findFromRootIdEq : GrainId -> GrainZipper -> Maybe GrainZipper
findFromRootIdEq gid =
    Z.findFromRoot (Grain.idEq gid)
        |> mapMaybe


removeTree : GrainZipper -> Maybe GrainZipper
removeTree =
    Z.removeTree
        |> mapMaybe


removeEqById : Grain -> GrainZipper -> Maybe GrainZipper
removeEqById grain =
    findFromRootEqById grain
        >> Maybe.andThen removeTree


removeEqByIdThenFlatten : Grain -> GrainZipper -> List Grain
removeEqByIdThenFlatten grain =
    removeEqById grain >> Maybe.unwrap [] flatten


flatten : GrainZipper -> List Grain
flatten =
    unwrap >> Z.toTree >> Tree.flatten


lastDescendantZ =
    Z.lastDescendant
        |> map


label =
    unwrap >> Z.label


lastDescendentGrain =
    lastDescendantZ >> label


appendGrain : Grain -> GrainZipper -> GrainZipper
appendGrain grain =
    Z.append (Tree.singleton grain)
        |> map


appendWhenIdEq : GrainId -> Grain -> GrainZipper -> Maybe GrainZipper
appendWhenIdEq gid grain =
    findFromRootIdEq gid
        >> Maybe.map (appendGrain grain)
