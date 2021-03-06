module GrainZipper__ exposing
    ( GrainForest
    , GrainTree
    , GrainZipper
    , appendWhenIdEqAndGetParentTree
    , backwardFromRootWhenIdEq
    , findTreeById
    , forwardFromRootWhenIdEq
    , fromTree
    , lastDescendentGrain
    , parentWhenIdEq
    , prependChildWhenIdEqAndGetParentTree
    , prependWhenIdEqAndGetParentTree
    , removeEqByIdThenFlatten
    , siblingsOf
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


tree =
    unwrap >> Z.tree


findTreeById : GrainId -> GrainZipper -> Maybe GrainTree
findTreeById gid =
    findFromRootIdEq gid >> Maybe.map tree


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


prependChildGrain : Grain -> GrainZipper -> Maybe GrainZipper
prependChildGrain grain =
    (Z.mapTree (Tree.prependChild (Tree.singleton grain))
        >> Z.firstChild
    )
        |> mapMaybe


prependGrain : Grain -> GrainZipper -> GrainZipper
prependGrain grain =
    Z.prepend (Tree.singleton grain)
        |> map


appendWhenIdEq : GrainId -> Grain -> GrainZipper -> Maybe GrainZipper
appendWhenIdEq gid grain =
    findFromRootIdEq gid
        >> Maybe.map (appendGrain grain)


prependWhenIdEq : GrainId -> Grain -> GrainZipper -> Maybe GrainZipper
prependWhenIdEq gid grain =
    findFromRootIdEq gid
        >> Maybe.map (prependGrain grain)


prependChildIdEq : GrainId -> Grain -> GrainZipper -> Maybe GrainZipper
prependChildIdEq gid grain =
    findFromRootIdEq gid
        >> Maybe.andThen (prependChildGrain grain)


appendWhenIdEqAndGetParentTree :
    GrainId
    -> Grain
    -> GrainZipper
    -> Maybe (Tree Grain)
appendWhenIdEqAndGetParentTree gid grain =
    appendWhenIdEq gid grain
        >> Maybe.andThen toParentChildrenLabelsTuple


prependWhenIdEqAndGetParentTree :
    GrainId
    -> Grain
    -> GrainZipper
    -> Maybe (Tree Grain)
prependWhenIdEqAndGetParentTree gid grain =
    prependWhenIdEq gid grain
        >> Maybe.andThen toParentChildrenLabelsTuple


prependChildWhenIdEqAndGetParentTree :
    GrainId
    -> Grain
    -> GrainZipper
    -> Maybe (Tree Grain)
prependChildWhenIdEqAndGetParentTree gid grain =
    prependChildIdEq gid grain
        >> Maybe.andThen toParentChildrenLabelsTuple


toParentChildrenLabelsTuple =
    parent >> Maybe.map tree


children =
    unwrap >> Z.children


forward =
    Z.forward
        |> mapMaybe


backward =
    Z.backward
        |> mapMaybe


parent =
    Z.parent
        |> mapMaybe


forwardFromRootWhenIdEq : GrainId -> GrainZipper -> Maybe Grain
forwardFromRootWhenIdEq gid =
    findFromRootIdEq gid
        >> Maybe.andThen forward
        >> Maybe.map label


backwardFromRootWhenIdEq : GrainId -> GrainZipper -> Maybe Grain
backwardFromRootWhenIdEq gid =
    findFromRootIdEq gid
        >> Maybe.andThen backward
        >> Maybe.map label


parentWhenIdEq : GrainId -> GrainZipper -> Maybe Grain
parentWhenIdEq gid =
    findFromRootIdEq gid
        >> Maybe.andThen parent
        >> Maybe.map label


siblingsOf : GrainId -> GrainZipper -> List Grain
siblingsOf gid =
    findFromRootIdEq gid
        >> Maybe.andThen parent
        >> Maybe.unwrap []
            (unwrap
                >> Z.children
                >> List.map Tree.label
            )
